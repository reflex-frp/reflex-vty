-- Manual spec registration: this suite does not use hspec-discover
-- (see CONTRIBUTING.md). Register each new *Spec.hs module here and
-- in the test suite's other-modules in reflex-vty.cabal.

import System.Environment (lookupEnv)
import Test.Hspec

import qualified Data.Text.ZipperSpec
import qualified Reflex.Vty.CanvasSpec
import qualified Reflex.Vty.ColorProfileSpec
import qualified Reflex.Vty.ColorSpec
import qualified Reflex.Vty.HostSpec
import qualified Reflex.Vty.StyleJoinSpec
import qualified Reflex.Vty.StyleSpec
import qualified Reflex.Vty.Test.GoldenPolicySpec
import qualified Reflex.Vty.Test.GoldenSpec
import Reflex.Vty.Test.Snapshot (GoldenMode (..))
import qualified Reflex.Vty.Test.SnapshotSpec
import qualified Reflex.Vty.Widget.BoxSpec
import qualified Reflex.Vty.Widget.CursorSpec
import qualified Reflex.Vty.Widget.MouseSpec
import qualified Reflex.Vty.Widget.ScrollSpec

main :: IO ()
main = do
  goldenMode <- goldenModeFromEnv
  hspec (spec goldenMode)

-- | Read REFLEX_VTY_GOLDEN_UPDATE once at startup. An absent value or
-- @0@ selects checking. @1@ selects updating and is forbidden when
-- @CI=true@. Any other value is a configuration error.
goldenModeFromEnv :: IO GoldenMode
goldenModeFromEnv = do
  value <- lookupEnv "REFLEX_VTY_GOLDEN_UPDATE"
  ci <- lookupEnv "CI"
  case value of
    Nothing -> pure CheckGoldens
    Just "0" -> pure CheckGoldens
    Just "1"
      | ci == Just "true" ->
          errorWithoutStackTrace "REFLEX_VTY_GOLDEN_UPDATE=1 is not allowed when CI=true"
      | otherwise -> pure UpdateGoldens
    Just other ->
      errorWithoutStackTrace $
        "invalid REFLEX_VTY_GOLDEN_UPDATE value "
          ++ show other
          ++ ": expected it to be absent, 0, or 1"

spec :: GoldenMode -> Spec
spec goldenMode = do
  describe "Data.Text.ZipperSpec" Data.Text.ZipperSpec.spec
  describe "Reflex.Vty.CanvasSpec" Reflex.Vty.CanvasSpec.spec
  describe "Reflex.Vty.ColorProfileSpec" Reflex.Vty.ColorProfileSpec.spec
  describe "Reflex.Vty.ColorSpec" Reflex.Vty.ColorSpec.spec
  describe "Reflex.Vty.HostSpec" Reflex.Vty.HostSpec.spec
  describe "Reflex.Vty.StyleSpec" Reflex.Vty.StyleSpec.spec
  describe "Reflex.Vty.StyleJoinSpec" Reflex.Vty.StyleJoinSpec.spec
  describe "Reflex.Vty.Test.GoldenPolicySpec" Reflex.Vty.Test.GoldenPolicySpec.spec
  describe "Reflex.Vty.Test.SnapshotSpec" Reflex.Vty.Test.SnapshotSpec.spec
  describe "Reflex.Vty.Widget.BoxSpec" Reflex.Vty.Widget.BoxSpec.spec
  describe "Reflex.Vty.Widget.CursorSpec" Reflex.Vty.Widget.CursorSpec.spec
  describe "Reflex.Vty.Widget.MouseSpec" Reflex.Vty.Widget.MouseSpec.spec
  describe "Reflex.Vty.Widget.ScrollSpec" Reflex.Vty.Widget.ScrollSpec.spec
  describe "Reflex.Vty.Test.GoldenSpec" (Reflex.Vty.Test.GoldenSpec.spec goldenMode)
