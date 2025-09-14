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
import Control.Monad ((<=<), join, when)
import Data.Functor.Misc (Const2(..))

import Reflex
import Reflex.EventWriter.Base
import Reflex.Network
import Reflex.Patch.MapWithMove
import Test.Run

main :: IO ()
main = do
  b1s <- runAppB testPatchMapWithMove $ map Just [Increment 'b', Increment 'b',       Increment 'd', Increment 'b']
  mapM_ print b1s
  let !True = last (last b1s) == ["0a0","1b3","2c0","3d1","4e0"]

  b2s <- runAppB testPatchMapWithMove $ map Just [Increment 'b', Increment 'b', Swap, Increment 'd', Increment 'b']
  mapM_ print b2s
  let !True = last (last b2s) == ["0a0","3d1","2c0","1b3","4e0"]

  return ()

data PatchMapTestAction
  = Increment Char
  | Swap
  | Update
  | Update2
  deriving (Eq, Show)

testPatchMapWithMove
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
testPatchMapWithMove pulse = do
  performEvent_ $ ffor pulse $ \p -> liftIO . putStrLn $ "pulse " <> show p

  let
    mapAction = fforMaybe pulse $ \case
      Increment _ -> Nothing
      Swap -> patchMapWithMove $ Map.fromList
        [ (1, NodeInfo (From_Move 3) (Just 3))
        , (3, NodeInfo (From_Move 1) (Just 1))
        ]
      Update -> patchMapWithMove $ Map.fromList
        [ (1, NodeInfo (From_Insert 'z') Nothing) ]
      Update2 -> patchMapWithMove $ Map.fromList
        [ (3, NodeInfo (From_Insert 'y') Nothing) ]

    incrementAction = fforMaybe pulse $ \case
      Increment v -> Just $ Map.singleton v ()
      _ -> Nothing

    incrementSelector = fanMap incrementAction

  let
    child k v = mdo
      liftIO . putStrLn $ "child " <> show k <> [v]

      let eIncrement = select incrementSelector $ Const2 v

      -- Without this performEvent the tests pass. The issue is that the response from this performEvent request
      -- arrives to the other list item (the one that has been swapped with).
      eCounter <- performEvent $ ffor (current dCounter <@ eIncrement) $ \(c :: Int) -> do
        liftIO . putStrLn $ "child performEvent eCounter " <> show k <> [v] <> show c
        pure (c + 1)
      -- let eCounter = (+ 1) <$> current dCounter <@ eIncrement

      dCounter <- holdDyn 0 eCounter

      tellBehavior $ singleton . (\c -> show k <> [v] <> show c) <$> current dCounter

      performEvent_ $ ffor (updated dCounter) $ \c -> do
        liftIO . putStrLn $ "child performEvent dCounter " <> show k <> [v] <> show c

      pure $ "child" <> show k <> [v]

  (_, result) <- runBehaviorWriterT $ mdo
    (r0, r') <- mapMapWithAdjustWithMove
      child
      (Map.fromList $ zip [(0 :: Int)..] "abcde")
      mapAction

    let tracePatch (PatchMapWithMove m) = "r' " <> show m
    r <- holdIncremental r0 $ traceEventWith tracePatch r'

    performEvent_ . ffor (updated $ fromJust . Map.lookup 1 <$> incrementalToDynamic r) $ liftIO . putStrLn . ("result1 " <>)
    performEvent_ . ffor (updated $ fromJust . Map.lookup 3 <$> incrementalToDynamic r) $ liftIO . putStrLn . ("result3 " <>)
    performEvent_ . ffor (updated $ fromJust . Map.lookup 0 <$> incrementalToDynamic r) $ liftIO . putStrLn . ("result0 " <>)

    performEvent_ . ffor (updated $ incrementalToDynamic r) $ liftIO . putStrLn . ("r " <>) . show

    return ()
  return result
