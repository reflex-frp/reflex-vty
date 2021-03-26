{-|
Module: Reflex.Vty.Widget.Input.Text
Description: Widgets for accepting text input from users and manipulating text within those inputs
-}
module Reflex.Vty.Widget.Input.Text
  ( module Reflex.Vty.Widget.Input.Text
  , def
  ) where

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.NodeId (MonadNodeId)
import Data.Default (Default(..))
import Data.Text (Text)
import Data.Text.Zipper
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout

-- | Configuration options for a 'textInput'. For more information on
-- 'TextZipper', see 'Data.Text.Zipper'.
data TextInputConfig t = TextInputConfig
  { _textInputConfig_initialValue :: TextZipper
  , _textInputConfig_modify :: Event t (TextZipper -> TextZipper)
  , _textInputConfig_tabWidth :: Int
  , _textInputConfig_display :: Dynamic t (Char -> Char)
  -- ^ Transform the characters in a text input before displaying them. This is useful, e.g., for
  -- masking characters when entering passwords.
  }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig empty never 4 (pure id)

-- | The output produced by text input widgets, including the text
-- value and the number of display lines (post-wrapping). Note that some
-- display lines may not be visible due to scrolling.
data TextInput t = TextInput
  { _textInput_value :: Dynamic t Text
  , _textInput_lines :: Dynamic t Int
  }

-- | A widget that allows text input
textInput
  :: (Reflex t, MonadHold t m, MonadFix m, HasVtyInput t m, HasFocus t m, HasDisplayRegion t m, HasImageWriter t m, HasDisplayRegion t m)
  => TextInputConfig t
  -> m (TextInput t)
textInput cfg = do
  i <- input
  f <- focus
  dh <- displayHeight
  dw <- displayWidth
  rec v <- foldDyn ($) (_textInputConfig_initialValue cfg) $ mergeWith (.)
        [ uncurry (updateTextZipper (_textInputConfig_tabWidth cfg)) <$> attach (current dh) i
        , _textInputConfig_modify cfg
        , let displayInfo = (,) <$> current rows <*> scrollTop
          in ffor (attach displayInfo click) $ \((dl, st), MouseDown _ (mx, my) _) ->
            goToDisplayLinePosition mx (st + my) dl
        ]
      click <- mouseDown V.BLeft
      let cursorAttrs = ffor f $ \x -> if x then cursorAttributes else V.defAttr
      let rows = (\w s c -> displayLines w V.defAttr c s)
            <$> dw
            <*> (mapZipper <$> _textInputConfig_display cfg <*> v)
            <*> cursorAttrs
          img = images . _displayLines_spans <$> rows
      y <- holdUniqDyn $ _displayLines_cursorY <$> rows
      let newScrollTop :: Int -> (Int, Int) -> Int
          newScrollTop st (h, cursorY)
            | cursorY < st = cursorY
            | cursorY >= st + h = cursorY - h + 1
            | otherwise = st
      let hy = attachWith newScrollTop scrollTop $ updated $ zipDyn dh y
      scrollTop <- hold 0 hy
      tellImages $ (\imgs st -> (:[]) . V.vertCat $ drop st imgs) <$> current img <*> scrollTop
  return $ TextInput
    { _textInput_value = value <$> v
    , _textInput_lines = length . _displayLines_spans <$> rows
    }

-- | A widget that allows multiline text input
multilineTextInput
  :: (Reflex t, MonadHold t m, MonadFix m, HasVtyInput t m, HasFocus t m, HasDisplayRegion t m, HasImageWriter t m)
  => TextInputConfig t
  -> m (TextInput t)
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

-- | Wraps a 'textInput' or 'multilineTextInput' in a tile. Uses
-- the computed line count to greedily size the tile when vertically
-- oriented, and uses the fallback width when horizontally oriented.
textInputTile
  :: (Monad m, MonadNodeId m, Reflex t, MonadFix m, MonadLayout t m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, HasImageWriter t m, HasDisplayRegion t m)
  => m (TextInput t)
  -> Dynamic t Int
  -> m (TextInput t)
textInputTile txt width = do
  o <- askOrientation
  rec t <- tile (Constraint_Fixed <$> sz) txt
      let sz = join $ ffor o $ \case
            Orientation_Column -> _textInput_lines t
            Orientation_Row -> width
  return t

-- | Default attributes for the text cursor
cursorAttributes :: V.Attr
cursorAttributes = V.withStyle V.defAttr V.reverseVideo

-- | Turn a set of display line rows into a list of images (one per line)
images :: [[Span V.Attr]] -> [V.Image]
images = map (V.horizCat . map spanToImage)

-- | Turn a set of display line rows into a single image
image :: [[Span V.Attr]] -> V.Image
image = V.vertCat . images

-- | Turn a 'Span' into an 'Graphics.Vty.Image'
spanToImage :: Span V.Attr -> V.Image
spanToImage (Span attrs t) = V.text' attrs t

-- | Default vty event handler for text inputs
updateTextZipper
  :: Int -- ^ Tab width
  -> Int -- ^ Page size
  -> V.Event -- ^ The vty event to handle
  -> TextZipper -- ^ The zipper to modify
  -> TextZipper
updateTextZipper tabWidth pageSize ev = case ev of
  -- Special characters
  V.EvKey (V.KChar '\t') [] -> tab tabWidth
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
  V.EvKey V.KPageUp [] -> pageUp pageSize
  V.EvKey V.KPageDown [] -> pageDown pageSize
  _ -> id
