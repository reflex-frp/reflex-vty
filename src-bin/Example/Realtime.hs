{-# LANGUAGE RecursiveDo #-}

-- |
--   Description: Background activity that never blocks input, a la the
--   Bubble Tea realtime example.
module Example.Realtime (realtime) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | Simulated local activities arrive from background actions while the
-- keyboard stays live. Each request forks exactly one finite action; the next
-- request is fed back from the completed result. Delays repeat: 300 ms, 700 ms,
-- 500 ms.
realtime :: Demo t m => m (Event t ())
realtime = do
  postBuild <- getPostBuild
  rec let requests = leftmost [1 <$ postBuild, (+ 1) <$> responses]
      responses <- performEventAsync $ ffor requests $ \n trigger -> void $ liftIO $ forkIO $ do
        threadDelay $ delays n
        trigger n
  received <- holdDyn (0 :: Integer) responses
  i <- input
  let keyEvents = fforMaybe i $ \case
        e@V.EvKey {} -> Just e
        _ -> Nothing
  lastKey <- holdDyn (T.pack "No key received.") (T.pack . show <$> keyEvents)
  tellImages $ current $ ffor ((,) <$> received <*> lastKey) $ \(n, k) ->
    [ V.vertCat
        [ V.text' (V.withStyle (V.withForeColor V.defAttr accent) V.bold) (T.pack "  Simulated background activity")
        , V.char V.defAttr ' '
        , V.text' V.defAttr ((T.pack "  Events received: ") <> T.pack (show n))
        , V.char V.defAttr ' '
        , V.text' V.defAttr ((T.pack "  ") <> k)
        , V.char V.defAttr ' '
        , V.text' (V.withForeColor V.defAttr dim) (T.pack "  Type a key. q or Ctrl+C: quit")
        ]
    ]
  quit
  where
    delays n = case (n - 1) `mod` (3 :: Integer) of
      0 -> 300000
      1 -> 700000
      _ -> 500000
