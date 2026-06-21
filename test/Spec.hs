-- hspec auto-discovery stuff
-- does not work with current CircleCI image so instead we do it manually for now
-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Data.Text.ZipperSpec
import qualified Reflex.Vty.ColorProfileSpec
import qualified Reflex.Vty.StyleSpec
import qualified Reflex.Vty.Test.SnapshotSpec
import qualified Reflex.Vty.Test.GoldenSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.Text.ZipperSpec" Data.Text.ZipperSpec.spec
  describe "Reflex.Vty.ColorProfileSpec" Reflex.Vty.ColorProfileSpec.spec
  describe "Reflex.Vty.StyleSpec" Reflex.Vty.StyleSpec.spec
  describe "Reflex.Vty.Test.SnapshotSpec" Reflex.Vty.Test.SnapshotSpec.spec
  describe "Reflex.Vty.Test.GoldenSpec" Reflex.Vty.Test.GoldenSpec.spec
