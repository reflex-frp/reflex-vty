{-# LANGUAGE PackageImports #-}
module Reflex.Vty.Widget.Layout.SpacingSpec (spec) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import qualified Reflex.Vty as Core
import Reflex.Vty.Host (defaultVtyAppConfig)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), gridHeight, gridRows)
import Data.Default (def)
import "reflex-vty" Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout
import Data.Default (def)
import "reflex-vty" Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout.Spacing

lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame driver = do
  fs <- driverFrames driver
  case reverse fs of
    (f : _) -> pure f
    [] -> fail "expected at least one frame"

spec :: Spec
spec = describe "Reflex.Vty.Widget.Layout.Spacing" $ do
  it "row of three flex children in width 12 with one-cell gaps gives 4/3/3" $
    withApp defaultVtyAppConfig (12, 1) (spacingApp False) $ \driver -> do
      f0 <- lastFrame driver
      -- Width 12: three flex + two 1-cell gaps = 10 cells to flex, so
      -- 4/3/3 with the remainder unevenly distributed to early children.
      T.take 4 (T.pack (gridRowText 0 (frameGrid f0))) `shouldBe` "AAAA"
      T.take 1 (T.drop 5 (T.pack (gridRowText 0 (frameGrid f0)))) `shouldBe` "B"
      T.take 1 (T.drop 9 (T.pack (gridRowText 0 (frameGrid f0)))) `shouldBe` "C"
      stopApp driver

  it "empty list produces no content or gaps" $
    withApp defaultVtyAppConfig (10, 2) (emptyApp False) $ \driver -> do
      f0 <- lastFrame driver
      T.all (== ' ') (T.pack (gridRowText 0 (frameGrid f0))) `shouldBe` True
      stopApp driver

-- A row of three labeled flex children with one-cell gaps.
spacingApp :: Bool -> TestApp () ()
spacingApp _ _ = do
  initManager_ $
    rowWithSpacing def {_spacingConfig_gap = pure 1} $
      map (grout flex . label) ['A', 'B', 'C']
  pure (pure (), never)

emptyApp :: Bool -> TestApp () ()
emptyApp _ _ = do
  initManager_ $ rowWithSpacing def {_spacingConfig_gap = pure 1} []
  pure (pure (), never)

label :: (Reflex t, Monad m, Core.HasImageWriter t m) => Char -> m ()
label c = tellImages (pure [V.string V.defAttr (replicate 4 c)])

-- Local copy of the core CompositionSpec grid helper (the core spec
-- module is not shared).
gridRowText :: Int -> Grid -> String
gridRowText y g
  | y < 0 || y >= gridHeight g = ""
  | otherwise = T.unpack . T.concat . map renderSpanText $ gridRows g !! y
