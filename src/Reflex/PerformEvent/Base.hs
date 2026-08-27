-- | This module provides 'PerformEventT', the standard implementation of
-- 'PerformEvent'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.PerformEvent.Base
  ( PerformEventT (..)
  , FireCommand (..)
  , runFireCommand
  , hostPerformEventTAndRead
  , hostPerformEventT
  ) where

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.Requester.Base.Internal
import Reflex.Requester.Class
import Reflex.EventWriter.Class
import Reflex.EventWriter.Base

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Traversable.WithIndex (itraverse)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Misc
import Data.Functor.Identity
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty.Deferred (NonEmptyDeferred)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup (sconcat))
import qualified Data.Semigroup as S
import Data.Unique.Tag.Local
import Data.Tuple

#if !MIN_VERSION_base(4,18,0)
import Control.Monad.Identity
#endif

-- | Fires events for the given 'EventTrigger's and then folds the caller's step
-- function over every resulting frame. Multiple frames can occur due to
-- 'Reflex.Performevent.Class.PerformEvent.performEvent' feeding back new occurrences.
--
-- The caller is able to exit early by returning @Left <exitValue>@, after which
-- no more effects are performed.
newtype FireCommand t m = FireCommand
  { runFireCommandAndRead
      :: forall stop acc
      .  [DSum (EventTrigger t) Identity]
      -> acc
      -> (acc -> ReadPhase m (Either stop acc))
      -> m (Either stop acc)
  }

-- | Like 'runFireCommandAndRead', but without reading any events.
runFireCommand :: MonadReflexHost t m => FireCommand t m -> [DSum (EventTrigger t) Identity] -> m ()
runFireCommand fc triggers = void $ runFireCommandAndRead fc triggers () (\_ -> pure $ Right ())

-- | Provides a basic implementation of 'PerformEvent'.  Note that, despite the
-- name, 'PerformEventT' is not an instance of 'MonadTrans'.
newtype PerformEventT t m a = PerformEventT { unPerformEventT :: RequesterT t (HostFrame t) Identity (HostFrame t) a }

deriving instance ReflexHost t => Functor (PerformEventT t m)
deriving instance ReflexHost t => Applicative (PerformEventT t m)
deriving instance ReflexHost t => Monad (PerformEventT t m)
deriving instance ReflexHost t => MonadFix (PerformEventT t m)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (PerformEventT t m)
deriving instance (ReflexHost t, MonadException (HostFrame t)) => MonadException (PerformEventT t m)
deriving instance (ReflexHost t, Monoid a) => Monoid (PerformEventT t m a)
deriving instance (ReflexHost t, S.Semigroup a) => S.Semigroup (PerformEventT t m a)
deriving instance (ReflexHost t, MonadCatch (HostFrame t)) => MonadCatch (PerformEventT t m)
deriving instance (ReflexHost t, MonadThrow (HostFrame t)) => MonadThrow (PerformEventT t m)
deriving instance (ReflexHost t, MonadMask (HostFrame t)) => MonadMask (PerformEventT t m)

instance (PrimMonad (HostFrame t), ReflexHost t) => PrimMonad (PerformEventT t m) where
  type PrimState (PerformEventT t m) = PrimState (HostFrame t)
  primitive = PerformEventT . lift . primitive

instance (ReflexHost t, Ref m ~ Ref IO, Monad (HostFrame t), PrimMonad (HostFrame t)) => PerformEvent t (PerformEventT t m) where
  type Performable (PerformEventT t m) = HostFrame t
  {-# INLINABLE performEvent_ #-}
  performEvent_ = PerformEventT . requesting_
  {-# INLINABLE performEvent #-}
  performEvent = PerformEventT . requestingIdentity

instance (ReflexHost t, PrimMonad (HostFrame t)) => Adjustable t (PerformEventT t m) where
  {-# INLINE runWithReplace #-}
  runWithReplace a0 a' = PerformEventT $ RequesterT $ do
    env@(_, _ :: TagGen (PrimState (HostFrame t)) FakeRequesterStatePhantom) <- RequesterInternalT ask
    let runA :: forall a. PerformEventT t m a -> HostFrame t (a, Event t (NonEmptyDeferred (RequestEnvelope FakeRequesterStatePhantom (HostFrame t))))
        runA (PerformEventT (RequesterT a)) = runEventWriterT $ runReaderT (unRequesterInternalT a) env
    (result0, requests0) <- lift $ runA a0
    newA <- requestingIdentity $ runA <$> a'
    requests <- switchHoldPromptOnly requests0 $ fmapCheap snd newA
    RequesterInternalT $ tellEvent requests
    pure (result0, fmapCheap fst newA)
  {-# INLINE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f a0 a' = PerformEventT $ RequesterT $ do
    env@(_, _ :: TagGen (PrimState (HostFrame t)) FakeRequesterStatePhantom) <- RequesterInternalT ask
    let runA :: forall a. PerformEventT t m a -> HostFrame t (a, Event t (NonEmptyDeferred (RequestEnvelope FakeRequesterStatePhantom (HostFrame t))))
        runA (PerformEventT (RequesterT a)) = runEventWriterT $ runReaderT (unRequesterInternalT a) env
    children' <- requestingIdentity $ itraverse (\k -> runA . f k) <$> a'
    children0 <- lift $ itraverse (\k -> runA . f k) a0
    let results0 = fmap fst children0
        requests0 = fmap snd children0
        results' = fmap fst <$> children'
        requests' = fmap snd `fmapCheap` children'
    requests <- switchHoldPromptOnlyIncremental mergeIntIncremental coincidencePatchIntMap requests0 requests'
    RequesterInternalT $ tellEvent $ fforMaybeCheap requests concatIntMapMaybe
    pure (results0, results')
  {-# INLINE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust (f :: forall a. k a -> v a -> PerformEventT t m (v' a)) (a0 :: DMap k v) a' = PerformEventT $ RequesterT $ do
    env@(_, _ :: TagGen (PrimState (HostFrame t)) FakeRequesterStatePhantom) <- RequesterInternalT ask
    let runA :: forall a. k a -> v a -> HostFrame t (Compose ((,) (Event t (NonEmptyDeferred (RequestEnvelope FakeRequesterStatePhantom (HostFrame t))))) v' a)
        runA k v = fmap (Compose . swap) $ runEventWriterT $ runReaderT (unRequesterInternalT a) env
          where (PerformEventT (RequesterT a)) = f k v
    children' <- requestingIdentity $ traversePatchDMapWithKey runA <$> a'
    children0 <- lift $ DMap.traverseWithKey runA a0
    let results0 = DMap.map (snd . getCompose) children0
        requests0 = weakenDMapWith (fst . getCompose) children0
        results' = mapPatchDMap (snd . getCompose) <$> children'
        requests' = weakenPatchDMapWith (fst . getCompose) `fmapCheap` children'
    requests <- switchHoldPromptOnlyIncremental mergeMapIncremental coincidencePatchMap requests0 requests'
    RequesterInternalT $ tellEvent $ fforMaybeCheap requests concatMapMaybe
    pure (results0, results')
  {-# INLINE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove (f :: forall a. k a -> v a -> PerformEventT t m (v' a)) (a0 :: DMap k v) a' = PerformEventT $ RequesterT $ do
    env@(_, _ :: TagGen (PrimState (HostFrame t)) FakeRequesterStatePhantom) <- RequesterInternalT ask
    let runA :: forall a. k a -> v a -> HostFrame t (Compose ((,) (Event t (NonEmptyDeferred (RequestEnvelope FakeRequesterStatePhantom (HostFrame t))))) v' a)
        runA k v = fmap (Compose . swap) $ runEventWriterT $ runReaderT (unRequesterInternalT a) env
          where (PerformEventT (RequesterT a)) = f k v
    children' <- requestingIdentity $ traversePatchDMapWithMoveWithKey runA <$> a'
    children0 <- lift $ DMap.traverseWithKey runA a0
    let results0 = DMap.map (snd . getCompose) children0
        requests0 = weakenDMapWith (fst . getCompose) children0
        results' = mapPatchDMapWithMove (snd . getCompose) <$> children'
        requests' = weakenPatchDMapWithMoveWith (fst . getCompose) `fmapCheap` children'
    requests <- switchHoldPromptOnlyIncremental mergeMapIncrementalWithMove coincidencePatchMapWithMove requests0 requests'
    RequesterInternalT $ tellEvent $ fforMaybeCheap requests concatMapMaybe
    pure (results0, results')

concatIntMapMaybe :: Semigroup a => IntMap a -> Maybe a
concatIntMapMaybe m = case IntMap.elems m of
  [] -> Nothing
  h : t -> Just $ sconcat $ h :| t

concatMapMaybe :: Semigroup a => Map k a -> Maybe a
concatMapMaybe m = case Map.elems m of
  [] -> Nothing
  h : t -> Just $ sconcat $ h :| t

instance ReflexHost t => MonadReflexCreateTrigger t (PerformEventT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = PerformEventT . lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PerformEventT $ lift $ newFanEventWithTrigger f

-- | Handles the frame lifecycle for a 'PerformEventT' program.
--
-- * At t₀, the host-setup function is run, and any initial 'Performable'
--   actions are evaluated outside of Reflex-time.
--
-- * At t₁, the results are fed back as occurrences to the 'performEvent' result
--   events.
--
-- * If these result occurrences produce more 'Performable' actions, these are
--   fed back as new occurrences at t₂, and so on, until no more actions are
--   produced.
--
-- If 'Performable' occurrences happen after the initial settle, the same loop
-- repeats. It returns the setup results, the folded observation of the frame-0
-- settle, and a 'FireCommand' for subsequent frames.

{-# INLINABLE hostPerformEventTAndRead #-}
hostPerformEventTAndRead :: forall t m a b acc0 stop0.
                     ( MonadReflexHost t m
                     , MonadRef m
                     , Ref m ~ Ref IO
                     , PrimMonad (HostFrame t)
                     )
                  => PerformEventT t m a
                  -> (a -> HostFrame t b)
                  -- ^ Setup on the t₀ result (e.g. do 'subscribeEvent' here).
                  -> acc0
                  -- ^ Initial value for the fold over the Perform-loop.
                  -> (b -> acc0 -> ReadPhase m (Either stop0 acc0))
                  -- ^ Perform-loop fold function.
                  -> m (a, b, Either stop0 acc0, FireCommand t m)
hostPerformEventTAndRead builder initialHostFrame seed step0 = do
  (response, responseTrigger) <- newEventWithTriggerRef
  let
    readStep :: forall stop' acc' request
             .  EventHandle t (RequestData (PrimState (HostFrame t)) request)
             -> (acc' -> ReadPhase m (Either stop' acc'))
             -> acc'
             -> ReadPhase m (Either stop' acc', Maybe (RequestData (PrimState (HostFrame t)) request))
    readStep perfHandle step acc = do
      ds <- step acc
      more <- sequence =<< readEvent perfHandle
      pure (ds, more)
    -- Fold the step across the cascade until it aborts or the cascade quiesces.
    drain :: forall stop' acc'
          .  EventHandle t (RequestData (PrimState (HostFrame t)) (HostFrame t))
          -> (acc' -> ReadPhase m (Either stop' acc'))
          -> (Either stop' acc', Maybe (RequestData (PrimState (HostFrame t)) (HostFrame t)))
          -> m (Either stop' acc')
    drain _ _ (Left stop, _) = pure $ Left stop
    drain _ _ (Right acc, Nothing) = pure $ Right acc
    drain perfHandle step (Right acc, Just toPerform) =
      drain perfHandle step =<< do
        mrt <- readRef responseTrigger
        hostFrameAndRead
          (traverseRequesterData (fmap Identity) toPerform)
          (\responses -> pure $ maybe [] (\rt -> [rt :=> Identity responses]) mrt)
          (const (readStep perfHandle step acc))
  (a, b, perfHandle, frame0) <- hostFrameAndRead
    (do (result, eventToPerform) <- runRequesterT (unPerformEventT builder) response
        perfHandle' :: EventHandle t (RequestData (PrimState (HostFrame t)) request) <- subscribeEvent eventToPerform
        b' <- initialHostFrame result
        pure (result, b', perfHandle'))
    (const (pure []))
    (\(result, b', perfHandle') -> (,,,) result b' perfHandle' <$> readStep perfHandle' (step0 b') seed)
  t0result <- drain perfHandle (step0 b) frame0
  pure
    ( a
    , b
    , t0result
    , FireCommand $ \triggers v step ->
        drain perfHandle step =<< fireEventsAndRead triggers (readStep perfHandle step v)
    )

-- | Like 'hostPerformEventTAndRead', but without observing the frame in which
-- the network is built.
{-# INLINABLE hostPerformEventT #-}
hostPerformEventT :: forall t m a.
                     ( MonadReflexHost t m
                     , MonadRef m
                     , Ref m ~ Ref IO
                     , PrimMonad (HostFrame t)
                     )
                  => PerformEventT t m a
                  -> m (a, FireCommand t m)
hostPerformEventT builder = do
  (a, _, _, fc) <- hostPerformEventTAndRead builder (const $ pure ()) () (\_ _ -> pure $ Right ())
  pure (a, fc)

instance ReflexHost t => MonadSample t (PerformEventT t m) where
  {-# INLINABLE sample #-}
  sample = PerformEventT . lift . sample

instance ReflexHost t => MonadHold t (PerformEventT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = PerformEventT $ lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = PerformEventT $ lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = PerformEventT $ lift $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 v' = PerformEventT $ lift $ buildDynamic getV0 v'
  {-# INLINABLE headE #-}
  headE = PerformEventT . lift . headE
  {-# INLINABLE now #-}
  now = PerformEventT . lift $ now

instance (MonadRef (HostFrame t), ReflexHost t) => MonadRef (PerformEventT t m) where
  type Ref (PerformEventT t m) = Ref (HostFrame t)
  {-# INLINABLE newRef #-}
  newRef = PerformEventT . lift . newRef
  {-# INLINABLE readRef #-}
  readRef = PerformEventT . lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = PerformEventT . lift . writeRef r

instance (MonadAtomicRef (HostFrame t), ReflexHost t) => MonadAtomicRef (PerformEventT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = PerformEventT . lift . atomicModifyRef r
