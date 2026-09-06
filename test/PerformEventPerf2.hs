{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}


module Main where

import Control.Monad.Fix
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((<=<), join, when, void, forM_)
import Data.Functor.Misc (Const2(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DM
import Data.GADT.Compare
import Data.GADT.Show
import Data.Typeable ((:~:)(Refl))

import Reflex
import Reflex.EventWriter.Base
import Reflex.Network
import Reflex.Patch.MapWithMove
import Test.Run


-- Dependent multimap
newtype DMMap k = DMMap { unDMMap :: DMap k NonEmpty }

instance GCompare a => Semigroup (DMMap a) where
  (DMMap a) <> (DMMap b) = DMMap $ DM.unionWithKey (const (<>)) a b

instance GCompare k => Monoid (DMMap k) where
  mempty  = DMMap DM.empty
  mconcat = DMMap . DM.unionsWithKey (const (<>)) . map unDMMap

singletonNE :: k v -> v -> DMMap k
singletonNE k = DMMap . DM.singleton k . (NE.:| [])


main :: IO ()
main = do
  start <- liftIO getCurrentTime
  b1s <- runAppB testRunWithReplace $ map Just (replicate 10000 $ Increment 'a')
  mapM_ print b1s
  let !False = last (last b1s) == ["0a0","1b3","2c0","3d1","4e0"]
  end <- liftIO getCurrentTime
  liftIO $ putStrLn $ "total runtime: " <> show (diffUTCTime end start)
  let !False = True
  return ()


data PatchMapTestAction
  = Increment Char
  | Swap
  | Update
  | Update2
  deriving (Eq, Show)

data AppCommand a where
  MouseMove :: AppCommand Int
  MouseDown :: AppCommand Int

instance GEq AppCommand where
  geq MouseMove MouseMove = Just Refl
  geq MouseDown MouseDown = Just Refl
  geq _   _   = Nothing

instance GCompare AppCommand where
  gcompare MouseMove MouseMove = GEQ
  gcompare MouseMove _   = GLT
  gcompare _   MouseMove = GGT

  gcompare MouseDown MouseDown = GEQ

instance Show (AppCommand a) where
  showsPrec _ MouseMove = showString "MouseMove"
  showsPrec _ MouseDown = showString "MouseDown"

instance GShow AppCommand where
  gshowsPrec = showsPrec



testRunWithReplace
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m

     , MonadIO m
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => Event t PatchMapTestAction
  -> m (Behavior t [String])
testRunWithReplace pulse = mdo
  let
    initialCards = [0..600]
    initialCardsV = V.fromList initialCards
    initialCardsM = M.fromList [(k, k) | k <- initialCards]

  -- performEvent_ $ ffor pulse $ \p -> liftIO . putStrLn $ "pulse " <> show p

  let
    esDebugHoverBoxAlpha = fanMap $ ffor eMouseMove $ \i -> M.singleton i ()

  let
    cCard = cDebugBox
    -- cCard index = do
    --   listHoldWithKey (M.singleton 0 0) never $ \_ _ -> cCell index

    -- cCell index = do
    --   snd <$> runWithReplace (cDebugBox index) never

    cDebugBox index = {-# SCC "cDebugBox" #-} do
      let eDebugHoverBoxAlpha = select esDebugHoverBoxAlpha (Const2 index)
      -- performEvent_ $ ffor eDebugHoverBoxAlpha $ const . liftIO $ putStrLn "selectDebugHoverBoxAlpha"

      void $ runWithReplace (pure ()) $ ffor eDebugHoverBoxAlpha $ \_ -> {-# SCC "cDebugBox_inner" #-} do
        start <- liftIO getCurrentTime

        -- {-# SCC "cDebugBox_inner_runWithReplace" #-} do
        --   {-# SCC "cDebugBox_inner_test" #-} do
        --     liftIO $ putStrLn "test"
        --   void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        --   void $ runWithReplace (pure ()) $ ffor never (const $ pure ())

        end <- liftIO getCurrentTime
        liftIO $ putStrLn $ show (diffUTCTime end start)
        pure ()

    cMouseEvents = do
      eMotionOcc :: Event t Int <- fmap fst <$> numberOccurrences pulse
      let eMove = ffor eMotionOcc $ \i -> initialCardsV V.! (i `mod` V.length initialCardsV)
      tellEvent $ mergeWith (<>) [singletonNE MouseMove <$> eMove]

  (_, eCommands) <- runEventWriterT $ do
    cMouseEvents
    -- forM_ initialCards cCard
    listHoldWithKey initialCardsM never $ \i _ -> cCard i
  let
    esCommand = fanG $ unDMMap <$> eCommands
    eMouseMove = head . NE.toList <$> selectG esCommand MouseMove

  -- performEvent_ $ ffor eCommands $ \p -> liftIO . putStrLn $ "eCommands" <> show p

  return $ constant []
