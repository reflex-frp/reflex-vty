-- | Input-policy checks for the interactive gallery examples. Each test runs
-- the real production host through the 'withApp' harness, counts the
-- shutdown requests the example returns, and asserts the input policy. The
-- count is observed through 'stepInput', so no frame from a stopped host is
-- awaited.
module Example.InteractionSpec (spec) where

import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Example.PreventQuit (preventQuit)
import Example.PrintKey (printKey)
import Reflex.Vty
  ( HasColorProfile
  , HasCursor
  , HasDisplayRegion
  , HasFocusReader
  , HasImageWriter
  , HasInput
  , HasScreenMode
  , HasTheme
  , MonadNodeId
  , MonadVtyApp
  , defaultVtyAppConfig
  )
import Reflex.Vty.Test.AppHarness

-- | Runs an example in the production host. The example's returned shutdown
-- event feeds a 'foldDyn' counter exposed as the observation; the harness's
-- own shutdown channel stays 'never'.
wrapExample
  :: (forall t m. TestWidget t m => m (Event t ()))
  -> TestApp () Int
wrapExample widget _ = do
  shutdown <- widget
  count <- foldDyn (\() n -> n + 1) (0 :: Int) shutdown
  pure (count, never)

-- | Deliver an input event and return the observed shutdown count from the
-- frame in which it took effect.
step :: AppDriver () Int -> V.Event -> IO Int
step d e = frameObservation <$> stepInput d e

ctrlcEvent :: V.Event
ctrlcEvent = V.EvKey (V.KChar 'c') [V.MCtrl]

spec :: Spec
spec = describe "Example.InteractionSpec" $ do
  let cfg = defaultVtyAppConfig

  describe "prevent-quit" $ do
    it "an affirmative key before a request emits no shutdown" $
      withApp cfg (80, 8) (wrapExample preventQuit) $ \d -> do
        n <- step d (V.EvKey (V.KChar 'y') [])
        n `shouldBe` 0
        stopApp d

    it "q then y emits exactly one shutdown" $
      withApp cfg (80, 8) (wrapExample preventQuit) $ \d -> do
        _ <- step d (V.EvKey (V.KChar 'q') [])
        n <- step d (V.EvKey (V.KChar 'y') [])
        n `shouldBe` 1
        stopApp d

    it "Ctrl+C opens the same confirmation and y confirms" $
      withApp cfg (80, 8) (wrapExample preventQuit) $ \d -> do
        _ <- step d ctrlcEvent
        n <- step d (V.EvKey (V.KChar 'y') [])
        n `shouldBe` 1
        stopApp d

    it "a repeated q while confirming still emits none" $
      withApp cfg (80, 8) (wrapExample preventQuit) $ \d -> do
        _ <- step d (V.EvKey (V.KChar 'q') [])
        _ <- step d (V.EvKey (V.KChar 'q') [])
        n <- step d ctrlcEvent
        n `shouldBe` 0
        stopApp d

    it "each cancellation key blocks the next unmodified y, but Y confirms" $
      withApp cfg (80, 8) (wrapExample preventQuit) $ \d -> do
        mapM_
          ( \cancel -> do
              before <- frameObservation . last <$> driverFrames d
              _ <- step d (V.EvKey (V.KChar 'q') [])
              _ <- step d cancel
              n0 <- step d (V.EvKey (V.KChar 'y') [])
              n0 `shouldBe` before
              _ <- step d (V.EvKey (V.KChar 'q') [])
              n1 <- step d (V.EvKey (V.KChar 'Y') [])
              n1 `shouldBe` before + 1
          )
          [ V.EvKey (V.KChar 'n') []
          , V.EvKey (V.KChar 'N') []
          , V.EvKey V.KEnter []
          , V.EvKey V.KEsc []
          ]
        stopApp d

    it "a modified y must not confirm" $
      withApp cfg (80, 8) (wrapExample preventQuit) $ \d -> do
        _ <- step d (V.EvKey (V.KChar 'q') [])
        n <- step d (V.EvKey (V.KChar 'y') [V.MCtrl])
        n `shouldBe` 0
        stopApp d

    it "an unrelated key while confirming leaves the next unmodified y able to confirm" $
      withApp cfg (80, 8) (wrapExample preventQuit) $ \d -> do
        _ <- step d (V.EvKey (V.KChar 'q') [])
        _ <- step d (V.EvKey (V.KChar 'x') [])
        n <- step d (V.EvKey (V.KChar 'y') [])
        n `shouldBe` 1
        stopApp d

  describe "print-key" $ do
    it "q, Escape, and Ctrl+A emit no shutdown" $
      withApp cfg (80, 8) (wrapExample printKey) $ \d -> do
        _ <- step d (V.EvKey (V.KChar 'q') [])
        _ <- step d (V.EvKey V.KEsc [])
        n <- step d (V.EvKey (V.KChar 'a') [V.MCtrl])
        n `shouldBe` 0
        stopApp d

    it "Ctrl+C emits exactly one shutdown" $
      withApp cfg (80, 8) (wrapExample printKey) $ \d -> do
        n <- step d ctrlcEvent
        n `shouldBe` 1
        stopApp d
