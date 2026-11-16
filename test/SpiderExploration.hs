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
  b1s <- runAppB app [Just (TestAction i) | i <- [1..1]]
  mapM_ print b1s
  -- let !False = last b1s == ["0a0","1b3","2c0","3d1","4e0"]
  end <- liftIO getCurrentTime
  liftIO $ putStrLn $ "total runtime: " <> show (diffUTCTime end start)
  -- let !False = True
  return ()

data Action
  = TestAction Int
  deriving (Eq, Show)

app
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     , MonadIO m
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => Event t Action
  -> m (Behavior t String)
app pulse = do
  performEvent_ $ ffor pulse $ \p -> liftIO . putStrLn $ "pulse " <> show p
  hold "0" $ ffor pulse $ \(TestAction x) -> show x
