-- hspec auto-discovery stuff
-- does not work with current CircleCI image so instead we do it manually for now
-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Data.Text.ZipperSpec
import qualified Reflex.Vty.CanvasSpec
import qualified Reflex.Vty.ColorProfileSpec
import qualified Reflex.Vty.ColorSpec
import qualified Reflex.Vty.HostSpec
import qualified Reflex.Vty.StyleJoinSpec
import qualified Reflex.Vty.StyleSpec
import qualified Reflex.Vty.Test.GoldenSpec
import qualified Reflex.Vty.Test.SnapshotSpec
import qualified Reflex.Vty.Widget.BoxSpec
import qualified Reflex.Vty.Widget.CursorSpec
import qualified Reflex.Vty.Widget.MouseSpec
import qualified Reflex.Vty.Widget.ScrollSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.Text.ZipperSpec" Data.Text.ZipperSpec.spec
  describe "Reflex.Vty.CanvasSpec" Reflex.Vty.CanvasSpec.spec
  describe "Reflex.Vty.ColorProfileSpec" Reflex.Vty.ColorProfileSpec.spec
  describe "Reflex.Vty.ColorSpec" Reflex.Vty.ColorSpec.spec
  describe "Reflex.Vty.HostSpec" Reflex.Vty.HostSpec.spec
  describe "Reflex.Vty.StyleSpec" Reflex.Vty.StyleSpec.spec
  describe "Reflex.Vty.StyleJoinSpec" Reflex.Vty.StyleJoinSpec.spec
  describe "Reflex.Vty.Test.SnapshotSpec" Reflex.Vty.Test.SnapshotSpec.spec
  describe "Reflex.Vty.Test.GoldenSpec" Reflex.Vty.Test.GoldenSpec.spec
  describe "Reflex.Vty.Widget.BoxSpec" Reflex.Vty.Widget.BoxSpec.spec
  describe "Reflex.Vty.Widget.CursorSpec" Reflex.Vty.Widget.CursorSpec.spec
  describe "Reflex.Vty.Widget.MouseSpec" Reflex.Vty.Widget.MouseSpec.spec
  describe "Reflex.Vty.Widget.ScrollSpec" Reflex.Vty.Widget.ScrollSpec.spec
