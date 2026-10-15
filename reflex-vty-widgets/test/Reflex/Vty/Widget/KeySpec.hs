module Reflex.Vty.Widget.KeySpec (spec) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Reflex.Vty.Host (defaultVtyAppConfig)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Widget.Input.Text
import Reflex.Vty.Widget.Layout
import Reflex.Vty.Widget.Key

lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame driver = do
  fs <- driverFrames driver
  case reverse fs of
    (f : _) -> pure f
    [] -> fail "expected at least one frame"

spec :: Spec
spec = describe "Reflex.Vty.Widget.Key" $ do

  it "default binding fires exactly once per keystroke" $
    withApp defaultVtyAppConfig (20, 3) keyApp $ \driver -> do
      f0 <- lastFrame driver
      frameObservation f0 `shouldBe` (0 :: Int, "")
      _ <- stepInput driver (V.EvKey (V.KChar 'x') [])
      f1 <- stepInput driver (V.EvKey (V.KChar 'x') [])
      frameObservation f1 `shouldBe` (2 :: Int, "")
      stopApp driver

  it "withKeyMap removes only matched key events from the child" $
    withApp defaultVtyAppConfig (20, 3) keyApp $ \driver -> do
      _ <- stepInput driver (V.EvKey (V.KChar '\t') []) -- focus the tile
      _ <- stepInput driver (V.EvKey (V.KChar 'a') []) -- unmatched: child
      _ <- stepInput driver (V.EvKey (V.KChar 'x') []) -- matched: consumed
      f <- stepInput driver (V.EvKey (V.KChar 'b') [])
      frameObservation f `shouldBe` (1 :: Int, "ab")
      stopApp driver

  it "live remapping makes the old key inert and the new key active" $
    withApp defaultVtyAppConfig (20, 3) keyApp $ \driver -> do
      _ <- stepInput driver (V.EvKey (V.KChar 'x') [])
      _ <- stepCommand driver KeyRemap
      fOld <- stepInput driver (V.EvKey (V.KChar 'x') [])
      frameObservation fOld `shouldBe` (1 :: Int, "") -- 'x' is no longer bound
      fNew <- stepInput driver (V.EvKey (V.KChar 'i') [])
      frameObservation fNew `shouldBe` (2 :: Int, "")
      stopApp driver

  it "disabled bindings are not consumed and do not dispatch" $
    withApp defaultVtyAppConfig (20, 3) keyApp $ \driver -> do
      _ <- stepCommand driver KeyToggleEnable
      _ <- stepInput driver (V.EvKey (V.KChar '\t') [])
      f <- stepInput driver (V.EvKey (V.KChar 'x') [])
      frameObservation f `shouldBe` (0 :: Int, "x") -- child got the key
      stopApp driver

-- A counter driven by one increment binding, plus a text input
-- wrapped by 'withKeyMap'. The command stream remaps and disables the
-- increment action live.
keyApp :: TestApp KeyCmd (Int, T.Text)
keyApp cmdEv = do
  remapD <-
    foldDyn
      (const not)
      False
      (fforMaybe cmdEv (\c -> if c == KeyRemap then Just () else Nothing))
  enabledD <-
    foldDyn
      (const not)
      True
      (fforMaybe cmdEv (\c -> if c == KeyToggleEnable then Just () else Nothing))
  let kmD =
        ( \remapped enabled ->
            KeyMap
              [ KeyBinding
                  { _keyBinding_action = ()
                  , _keyBinding_keys = [(V.KChar (if remapped then 'i' else 'x'), [])]
                  , _keyBinding_help = "increment"
                  , _keyBinding_enabled = enabled
                  }
              ]
        )
          <$> remapD
          <*> enabledD
  (actions, ti) <- withKeyMap kmD $ do
    initManager_ $ do
      tabNavigation
      tile flex (textInput def)
  n <- count actions
  pure ((,) <$> n <*> _textInput_value ti, never)

data KeyCmd = KeyRemap | KeyToggleEnable
  deriving (Eq, Show)
