{-|
Module: Reflex.Vty.Widget.Input.Text
Description: Widgets for accepting text input from users and manipulating text within those inputs
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Vty.Widget.Input.Text
  ( module Reflex.Vty.Widget.Input.Text
  , def
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Default (Default(..))
import Data.Text (Text)
import Data.Text.Zipper
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Widget

-- | Configuration options for a 'textInput'. For more information on
-- 'TextZipper', see 'Data.Text.Zipper'.
data TextInputConfig t = TextInputConfig
  { _textInputConfig_initialValue :: TextZipper
  , _textInputConfig_modify :: Event t (TextZipper -> TextZipper)
  }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig empty never

-- | A widget that allows text input
textInput
  :: (Reflex t, MonadHold t m, MonadFix m)
  => TextInputConfig t
  -> VtyWidget t m (Dynamic t Text)
textInput cfg = do
  i <- input
  f <- focus
  rec v <- foldDyn ($) (_textInputConfig_initialValue cfg) $ mergeWith (.)
        [ updateTextZipper <$> i
        , _textInputConfig_modify cfg
        , let displayInfo = (,) <$> rows <*> scrollTop
          in ffor (attach (current displayInfo) click) $ \((dl, st), MouseDown _ (mx, my) _) ->
            goToDisplayLinePosition mx (st + my) dl
        ]
      dw <- displayWidth
      dh <- displayHeight
      click <- mouseDown V.BLeft
      let cursorAttrs = ffor f $ \x -> if x then cursorAttributes else V.defAttr
      let rows = (\w s c -> displayLines w V.defAttr c s) <$> dw <*> v <*> cursorAttrs
          img = images . _displayLines_spans <$> rows
      y <- holdUniqDyn $ _displayLines_cursorY <$> rows
      let newScrollTop :: Int -> (Int, Int) -> Int
          newScrollTop st (h, cursorY)
            | cursorY < st = cursorY
            | cursorY >= st + h = cursorY - h + 1
            | otherwise = st
      rec let hy = attachWith newScrollTop (current scrollTop) $ updated $ zipDyn dh y
          scrollTop <- holdDyn 0 hy
      tellImages $ (\imgs st -> (:[]) . V.vertCat $ drop st imgs) <$> current img <*> current scrollTop
  return $ value <$> v

-- | A widget that allows multiline text input
multilineTextInput
  :: (Reflex t, MonadHold t m, MonadFix m)
  => TextInputConfig t
  -> VtyWidget t m (Dynamic t Text)
multilineTextInput cfg = do
  i <- input
  textInput $ cfg
    { _textInputConfig_modify = mergeWith (.)
      [ fforMaybe i $ \case
          V.EvKey V.KEnter [] -> Just $ insert "\n"
          _ -> Nothing
      , _textInputConfig_modify cfg
      ]
    }

-- | Default attributes for the text cursor
cursorAttributes :: V.Attr
cursorAttributes = V.withStyle V.defAttr V.reverseVideo

-- | Turn a set of display line rows into a list of images (one per line)
images :: [[Span V.Attr]] -> [V.Image]
images = map (V.horizCat . map spanToImage)

-- | Turn a set of display line rows into a single image
image :: [[Span V.Attr]] -> V.Image
image = V.vertCat . images

-- | Turn a 'Span' into an 'Image'
spanToImage :: Span V.Attr -> V.Image
spanToImage (Span attrs t) = V.text' attrs t

-- | Default vty event handler for text inputs
updateTextZipper :: V.Event -> TextZipper -> TextZipper
updateTextZipper ev = case ev of
  -- Regular characters
  V.EvKey (V.KChar k) [] -> insertChar k
  -- Deletion buttons
  V.EvKey V.KBS [] -> deleteLeft
  V.EvKey V.KDel [] -> deleteRight
  -- Key combinations
  V.EvKey (V.KChar 'u') [V.MCtrl] -> const empty
  V.EvKey (V.KChar 'w') [V.MCtrl] -> deleteLeftWord
  -- Arrow keys
  V.EvKey V.KLeft [] -> left
  V.EvKey V.KRight [] -> right
  V.EvKey V.KUp [] -> up
  V.EvKey V.KDown [] -> down
  V.EvKey V.KHome [] -> home
  V.EvKey V.KEnd [] -> end
  _ -> id
