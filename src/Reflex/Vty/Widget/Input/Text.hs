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
import Data.Default (Default(..))
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Zipper
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout
import Reflex.Vty.Widget.Input.Mouse

-- | Configuration options for a 'textInput'. For more information on
-- 'TextZipper', see 'Data.Text.Zipper'.
data TextInputConfig t = TextInputConfig
  { _textInputConfig_initialValue :: TextZipper
  -- ^ Initial value. This is a 'TextZipper' because it is more flexible
  -- than plain 'Text'. For example, this allows to set the Cursor position,
  -- by choosing appropriate values for '_textZipper_before' and '_textZipper_after'.
  , _textInputConfig_modify :: Event t (TextZipper -> TextZipper)
  -- ^ Modify the UI Event chain.
  --
  -- For example, the following 'TextInputConfig' inserts an additional 'a'
  -- when the letter 'b' is entered into 'textInput':
  --
  -- @
  --   i <- input
  --   textInput def
  --     { _textInputConfig_modify = fforMaybe i $ \case
  --         V.EvKey (V.KChar 'b') _ -> Just (insert "a")
  --         _ -> Nothing
  --     }
  -- @
  , _textInputConfig_setValue :: Maybe (Event t TextZipper)
  -- ^ Optionally set the value of the textInput field.
  --
  -- If set, this will override any Events sent by the UI,
  -- events from '_textInput_userInput' are no longer automatically applied
  -- to the textInput.
  , _textInputConfig_tabWidth :: Int
  , _textInputConfig_display :: Dynamic t (Char -> Char)
  -- ^ Transform the characters in a text input before displaying them. This is useful, e.g., for
  -- masking characters when entering passwords.
  }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig empty never Nothing 4 (pure id)

-- | The output produced by text input widgets, including the text
-- value and the number of display lines (post-wrapping). Note that some
-- display lines may not be visible due to scrolling.
data TextInput t = TextInput
  { _textInput_value :: Dynamic t Text
  -- ^ The current value of the textInput as Text.
  , _textInput_userInput :: Event t TextZipper
  -- ^ UI Event updates with the current 'TextZipper'.
  -- This does not include Events added by '_textInputConfig_setValue', but
  -- it does include '_textInputConfig_modify' Events.
  , _textInput_lines :: Dynamic t Int
  }

-- | A widget that allows text input
textInput
  :: (Reflex t, MonadHold t m, MonadFix m, HasInput t m, HasFocusReader t m, HasTheme t m, HasDisplayRegion t m, HasImageWriter t m, HasDisplayRegion t m)
  => TextInputConfig t
  -> m (TextInput t)
textInput cfg = do
  i <- input
  f <- focus
  dh <- displayHeight
  dw <- displayWidth
  bt <- theme
  attr0 <- sample bt
  rec
      -- we split up the events from vty and the one users provide to avoid cyclical
      -- update dependencies. This way, users may subscribe only to UI updates.
      valueChangedBySetValue <- case _textInputConfig_setValue cfg of
        Nothing -> pure never
        Just eSetValue -> pure $ fmap const eSetValue
      let valueChangedByUI = mergeWith (.)
            [ uncurry (updateTextZipper (_textInputConfig_tabWidth cfg)) <$> attach (current dh) i
            , _textInputConfig_modify cfg
            , let displayInfo = (,) <$> current rows <*> scrollTop
              in ffor (attach displayInfo click) $ \((dl, st), MouseDown _ (mx, my) _) ->
                goToDisplayLinePosition mx (st + my) dl
            ]
      v <- foldDyn ($) (_textInputConfig_initialValue cfg) $ mergeWith (.)
        [ valueChangedBySetValue
        , valueChangedByUI
        ]
      click <- mouseDown V.BLeft

      -- TODO reverseVideo is prob not what we want. Does not work with `darkTheme` in example.hs (cursor is dark rather than light bg)
      let toCursorAttrs attr = V.withStyle attr V.reverseVideo
          rowInputDyn = (,,)
            <$> dw
            <*> (mapZipper <$> _textInputConfig_display cfg <*> v)
            <*> f
          toDisplayLines attr (w, s, x)  =
            let c = if x then toCursorAttrs attr else attr
            in displayLines w attr c s
      attrDyn <- holdDyn attr0 $ pushAlways (\_ -> sample bt) (updated rowInputDyn)
      let rows = ffor2 attrDyn rowInputDyn toDisplayLines
          img = images . _displayLines_spans <$> rows
      y <- holdUniqDyn $ fmap snd _displayLines_cursorPos <$> rows
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
    , _textInput_userInput = attachWith (&) (current v) valueChangedByUI
    , _textInput_lines = length . _displayLines_spans <$> rows
    }

-- | A widget that allows multiline text input
multilineTextInput
  :: (Reflex t, MonadHold t m, MonadFix m, HasInput t m, HasFocusReader t m, HasTheme t m, HasDisplayRegion t m, HasImageWriter t m)
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
  :: (Monad m, Reflex t, MonadFix m, HasLayout t m, HasInput t m, HasFocus t m, HasImageWriter t m, HasDisplayRegion t m, HasFocusReader t m, HasTheme t m)
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
