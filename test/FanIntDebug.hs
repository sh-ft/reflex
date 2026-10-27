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
import Data.IORef

import Reflex
import Reflex.EventWriter.Base
import Reflex.Network
import Reflex.FanTag
import Data.Unique.Tag.Local.Internal
import qualified Data.TagMap as TagMap
import Data.Dependent.Sum
import Test.Run

import System.Mem


maxIterations :: Int
maxIterations = 4

main :: IO ()
main = do
  b1s <- runAppB testFanInt $ Just <$> [1..maxIterations]
  -- mapM_ print b1s
  -- let !False = last (last b1s) == ["0a0","1b3","2c0","3d1","4e0"]
  let !False = True
  return ()

-- newtype MyTag t a = MyTag { unMyTag :: Event t a }
data MyTag t a where
  EventTag :: Event t a -> MyTag t (Event t a)
  IntTag :: Int -> MyTag t Int

unEventTag :: MyTag t (Event t a) -> Event t a
unEventTag (EventTag e) = e

unIntTag :: MyTag t Int -> Int
unIntTag (IntTag x) = x
  
testFanInt
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m

     , MonadIO m
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => Event t Int
  -> m (Behavior t [String])
testFanInt pulse = mdo
  performEvent_ $ ffor pulse $ \i -> liftIO $ putStrLn $ "pulse " <> show i

  -- # 1
  let
    eResponses = ffor pulse $ \i -> TagMap.fromList $
      [ unsafeTagFromId 0 :=> EventTag (selectTag esResponses (unsafeTagFromId i :: Tag t Int)) ]
        <> [unsafeTagFromId j :=> IntTag j | j <- [1..maxIterations]]
    esResponses = fanTag eResponses
    eInnerRequests = unEventTag <$> selectTag esResponses (unsafeTagFromId 0 :: Tag t (Event t Int))
  eRequests <- switchHold never eInnerRequests
  performEvent_ $ ffor eRequests $ const . liftIO $ putStrLn "requests"

  return $ constant []

-- # 1
-- runWithReplace $ ffor e do
--   w <- performEvent u -- tellEvent u
--   performEvent_ w     -- tellEvent (selectTag <u'>)
--
-- requests <- switchHold <=< requestingIdentity $ runEventWriter <$> e'
-- requests <- switchHold selectTag
--
-- innerRequests :: Event [req] = mergeInt
--   [ u
--   , selectTag <u'>
--   ]
-- requestStream :: Event (Event [req]) = selectTag &> innerRequests
-- requests :: Event [req] <- switchHold requestStream -- when the outer event of requestStream fires, switch unsubscribes from the old inner event and subscribes to the new one
-- tellEvent requests
--
-- tellEvent $ switchHold $ selectTag &> mergeInt [selectTag u']

-- # 2
-- runWithReplace $ ffor e do
--   runWithReplace $ ffor never (pure ())

