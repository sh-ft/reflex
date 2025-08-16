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

import Reflex
import Reflex.EventWriter.Base
import Reflex.Network
import Reflex.Patch.MapWithMove
import Test.Run

main :: IO ()
main = do
  let actions =  [ Increment, Update, Increment, Swap, Increment, Increment, Increment  ]
  os <- runAppB testPatchMapWithMove $ map Just actions
  -- If the final counter value in the adjusted widgets corresponds to the number of times it has
  -- been incremented, we know that the networks haven't broken.
  let expectedCount = length $ ffilter (== Increment) actions
  mapM_ print os
  print expectedCount
  let !True = last (last os) == [show expectedCount, show expectedCount] -- TODO re-enable this test after issue #369 has been resolved
  return ()

data PatchMapTestAction
  = Increment
  | Swap
  | Update
  | Update2
  deriving (Eq, Show)

-- See https://github.com/reflex-frp/reflex/issues/369 for the bug that this is testing.
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
  let pulseAction = ffor pulse $ \case
        Increment -> Nothing
        Swap -> patchMapWithMove $ Map.fromList
          [ (1, NodeInfo (From_Move 3) (Just 3))
          , (3, NodeInfo (From_Move 1) (Just 1))
          ]
        Update -> patchMapWithMove $ Map.fromList
          [ (1, NodeInfo (From_Insert 'z') Nothing) ]
        Update2 -> patchMapWithMove $ Map.fromList
          [ (3, NodeInfo (From_Insert 'y') Nothing) ]
  counter <- foldDyn (+) 1 $ fmapMaybe (\e -> if isNothing e then Just 1 else Nothing) pulseAction
  performEvent_ $ ffor pulse $ \p -> liftIO . putStrLn $ "pulse " <> show p
  let
    -- counterAction = ffor (updated counter) $ \t ->
    --   fromJust . patchMapWithMove $ Map.singleton 0 (NodeInfo (From_Insert t) Nothing)

    showItem k v n t = show k <> [v] <> show n <> show t

    child k v = do
      liftIO . putStrLn $ "child " <> show k <> [v]
      eCounter' <- performEvent $ ffor (updated counter) $ \t -> do
        liftIO . putStrLn $ "child performEvent counter " <> show k <> [v] <> "_" <> show t
        pure t
      -- BUG: this traceEventWith stops outputting 1z and 3d after the swap, but weirdly keeps outputting 1b.
      counter' <- holdDyn (-1) $ traceEventWith (\t -> "child eCounter' " <> show k <> [v] <> "_" <> show t) eCounter'

      -- BUG: this one also stops working, just like the one inside `c`.
      -- tellBehavior $ singleton . (\t -> "child " <> show k <> [v] <> "_" <> show t) <$> current counter'

      -- But this one doesn't! So it's just `counter'`.
      -- tellBehavior $ singleton . (\t -> "child " <> show k <> [v] <> "_" <> show t) <$> current counter

      -- BUG: this one also stops working. Maybe it's the hold that stops working?
      performEvent_ $ ffor (updated counter') $ \t -> liftIO . putStrLn $ "child performEvent counter' " <> show k <> [v] <> "_" <> show t

      -- However, if we do this instead, it will start working. Then it might be that `eCounter' <- performEvent`
      -- is being executed (since putStrLn is working), but its result isn't being output in the returned event.
      -- counter' <- holdDyn (-1) $ ffor (updated counter) $ \t -> t + 80

      -- BUG: Yes, that seems to be the case, since this one stops outputting. So it's not the hold, it's performEvent.
      performEvent $ ffor eCounter' $ \t -> do
        liftIO . putStrLn $ "child performEvent eCounter' " <> show k <> [v] <> "_" <> show t
        pure t

      -- That means networkHold isn't even necessary to reproduce the bug, though it does seem to suffer from the same issue as performEvent.
      -- d <- networkHold (c counter' 1) (c counter' <$> updated counter)
      -- performEvent_ $ ffor (updated d) $ \s -> liftIO . putStrLn $ "child result " <> s
      -- pure d

      pure $ "child" <> show k <> [v]
      where
        c counter' n = do
          liftIO . putStrLn $ "c " <> show k <> [v] <> show n

          -- BUG?: inside this and following performEvent calls `n` is 1 less than it should be, but the on the line above ("c ") it's printed correctly.
          -- No, it's not a bug, it's just that the networkHold switch happens simultaneously with this performEvent call (both depend on `updated counter/pulse`)
          -- and the performEvent call is originating from the previous `c` network.
          performEvent_ $ ffor pulse $ \p -> liftIO . putStrLn $ "c pulse " <> show p <> " "<> show k <> [v] <> show n

          -- If uncommented, eCounter' is printing correctly, counter' is updated correctly,
          -- but tellBehavior below always prints -1, which means it's using the new version promptly.
          -- 
          -- eCounter' <- performEvent $ ffor (updated counter) $ \t -> do
          --   liftIO . putStrLn $ "c counter " <> show k <> [v] <> show n <> show t
          --   pure $ t + 90
          -- counter' <- holdDyn (-1) eCounter'
          -- performEvent_ $ ffor (updated counter') $ \t -> liftIO . putStrLn $ "c counter' " <> show k <> [v] <> show n <> show t

          -- BUG: after the Swap, this behavior stops being updated and `current counter'` stays at 3.
          -- It means that while performEvent requests stay active (for the network version 3), tellBehavior and result aren't.
          -- tellBehavior $ singleton . showItem k v n <$> current counter'
          tellBehavior $ singleton . showItem k v n <$> current counter

          -- BUG: after the Swap, this result stops being output for k = 1 and 3.
          -- At the same time, the performEvent calls above keep outputting, but their n stays at 3, no longer increasing.
          -- It means the networkHold network stops being updated for the Updated+Swapped keys, though the version 3 is still active.
          pure $ show k <> [v] <> show n
  (_, result) <- runBehaviorWriterT $ mdo
    -- TODO: output n and confirm that it's also outdated (both through passing to tellBehavior and returning as result)
    -- TODO: test if EventWriter suffers from the same issue
    (r0, r') <- mapMapWithAdjustWithMove
      -- (\_ v -> networkHold
      --   (tellBehavior $ constant [0])
      --   ((\t -> tellBehavior $ constant [t]) <$> updated counter))

      -- (\k v -> networkHold
      --   (tellBehavior $ singleton . showItem k v 0 <$> current counter)
      --   ((\n -> tellBehavior $ singleton . showItem k v n <$> current counter) <$> updated counter))
      child

      -- (\_ _ -> mapMapWithAdjustWithMove
      --   (\_ t -> tellBehavior $ constant [t])
      --   (Map.singleton 0 0)
      --   counterAction
      -- )

      -- (\_ _ -> tellBehavior $ singleton <$> current counter)

      (Map.fromList $ zip [(0 :: Int)..] "abcde")
      (fmapMaybe id pulseAction)

    let
      tracePatch (PatchMapWithMove m) = "r' " <> show m
    r <- holdIncremental r0 $ traceEventWith tracePatch r'

    -- r' :: Event t (PatchMapWithMove Integer (Dynamic t String))

    -- performEvent_ . ffor (pushAlways (sample . current . fromJust . Map.lookup 0) $ updated (incrementalToDynamic r)) $ liftIO . print

    -- performEvent_ . ffor (updated $ fromJust . Map.lookup 1 =<< incrementalToDynamic r) $ liftIO . putStrLn . ("result1 " <>)
    -- performEvent_ . ffor (updated $ fromJust . Map.lookup 3 =<< incrementalToDynamic r) $ liftIO . putStrLn . ("result3 " <>)
    -- performEvent_ . ffor (updated $ fromJust . Map.lookup 0 =<< incrementalToDynamic r) $ liftIO . putStrLn . ("result0 " <>)

    performEvent_ . ffor (updated $ fromJust . Map.lookup 1 <$> incrementalToDynamic r) $ liftIO . putStrLn . ("result1 " <>)
    performEvent_ . ffor (updated $ fromJust . Map.lookup 3 <$> incrementalToDynamic r) $ liftIO . putStrLn . ("result3 " <>)
    performEvent_ . ffor (updated $ fromJust . Map.lookup 0 <$> incrementalToDynamic r) $ liftIO . putStrLn . ("result0 " <>)

    performEvent_ . ffor (updated $ incrementalToDynamic r) $ liftIO . putStrLn . ("r " <>) . show

    return ()
  return result
