{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Fix
import Data.Maybe
import qualified Data.Map as Map
import Data.List (singleton)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((<=<), join, when, void, forM_)
import Data.Functor.Misc (Const2(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Map.Strict as M

import Reflex
import Reflex.EventWriter.Base
import Reflex.Network
import Test.Run

main :: IO ()
main = do
  start <- liftIO getCurrentTime
  b1s <- runAppB testRunWithReplace $ map Just (replicate 10000 $ Increment 'a')
  -- b1s <- runAppB testRunWithReplace $ map Just (replicate 1 $ Increment 'a')
  -- b1s <- runAppB testMapWithAdjustWithMove$ map Just (replicate 10000 $ Increment 'a')
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
testRunWithReplace pulse = do
  let initialCards = [0..100000]
  -- let initialCards = [0..10]

  performEvent_ $ ffor pulse $ \p -> liftIO . putStrLn $ "pulse " <> show p

  let
    eToggle = void pulse
    eMouseMotion = void pulse
    esDebugHoverBoxAlpha = fanMap $ ffor eMouseMotion $ \_ -> M.singleton (head initialCards) ()

  let
    cBigCard index = do
      void $ runWithReplace (pure ()) $ ffor eMouseMotion $ \_ -> do
        start <- liftIO getCurrentTime

        void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())
        -- void $ runWithReplace (pure ()) $ ffor never (const $ pure ())

        end <- liftIO getCurrentTime
        liftIO $ putStrLn $ show (diffUTCTime end start)
        pure ()

    cSmallCard index = do
      -- let eDebugHoverBoxAlpha = select esDebugHoverBoxAlpha (Const2 index)
      -- performEvent_ $ ffor eDebugHoverBoxAlpha $ const . liftIO $ putStrLn "selectDebugHoverBoxAlpha"

      void $ runWithReplace (pure ()) $ ffor never (const $ pure ())

  void $ cBigCard $ head initialCards
  forM_ (tail initialCards) cSmallCard

  let result = constant []
  return result

testMapWithAdjustWithMove
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
testMapWithAdjustWithMove pulse = do
  let initialCards :: [Int] = [0..100000]
  -- let initialCards :: [Int] = [0..10]

  performEvent_ $ ffor pulse $ \p -> liftIO . putStrLn $ "pulse " <> show p

  let
    eToggle = void pulse
    eMouseMotion = void pulse
    esDebugHoverBoxAlpha = fanMap $ ffor eMouseMotion $ \_ -> M.singleton (head initialCards) ()

  let
    cBigCard index = do
      void $ runWithReplace (pure ()) $ ffor eMouseMotion $ \_ -> do
        start <- liftIO getCurrentTime

        void $ runWithReplace (pure ()) $ ffor never (const $ pure ())

        end <- liftIO getCurrentTime
        liftIO $ putStrLn $ show (diffUTCTime end start)
        pure ()

    cSmallCard index = do
      -- let eDebugHoverBoxAlpha = select esDebugHoverBoxAlpha (Const2 index)
      -- performEvent_ $ ffor eDebugHoverBoxAlpha $ const . liftIO $ putStrLn "selectDebugHoverBoxAlpha"

      void $ runWithReplace (pure ()) $ ffor never (const $ pure ())

  -- void $ cBigCard $ head initialCards
  -- forM_ (tail initialCards) cSmallCard

  (r0, r') <- mapMapWithAdjustWithMove
    (\k v -> cBigCard k)
    (Map.fromList $ [(0, 0)])
    never

  -- forM_ (tail initialCards) cSmallCard

  (r0, r') <- mapMapWithAdjustWithMove
    (\k v -> cSmallCard k)
    (Map.fromList $ zip (tail initialCards) (tail initialCards))
    never

  let result = constant []
  return result
