{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex.Vty

import App (greeting)

-- | A pink → violet → cyan brand gradient, as used by the splash example.
brandGradient :: Gradient1D
brandGradient =
  gradient1D
    [ (0.0, RGB 255 64 160)
    , (0.5, RGB 150 90 255)
    , (1.0, RGB 0 200 230)
    ]

-- | Render a single line of bold text whose characters are colored along a
-- horizontal true-color gradient, centered within the available width.
gradientLine
  :: (Reflex t, HasDisplayRegion t m, HasImageWriter t m)
  => Gradient1D -> Text -> m ()
gradientLine grad txt = do
  dw <- displayWidth
  tellImages $ ffor (current dw) $ \w -> [img w]
  where
    cs = T.unpack txt
    n = length cs
    colored =
      V.horizCat
        [ V.char (V.withStyle (V.withForeColor V.defAttr (fromRGB (sampleGradient1D grad p))) V.bold) c
        | (i, c) <- zip [0 :: Int ..] cs
        , let p = fromIntegral i / fromIntegral (max 1 (n - 1))
        ]
    img w =
      V.horizCat
        [V.text' V.defAttr (T.replicate (max 0 ((w - n) `div` 2)) " "), colored]

main :: IO ()
main = mainWidget def $ localTheme (const (pure draculaTheme)) $ initManager_ $ do
  tabNavigation
  ctrlCPressed <- ctrlc
  exitClicked <-
    boxStatic roundedBoxStyle $
      col $ do
        grout (fixed 2) blank
        grout (fixed 1) $ gradientLine brandGradient greeting
        grout (fixed 2) blank
        grout (fixed 1) $ text "Welcome to your new reflex-vty app."
        grout (fixed 1) $ text "Edit the sources and re-run to iterate."
        grout (fixed 2) blank
        btn <-
          tile (fixed 3) $
            textButtonStatic
              (def {_buttonConfig_focusStyle = pure roundedBoxStyle})
              "Exit"
        grout (fixed 2) blank
        pure btn
  pure $ leftmost [ctrlCPressed, exitClicked]
