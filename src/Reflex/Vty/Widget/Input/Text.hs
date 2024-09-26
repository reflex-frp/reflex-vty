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
import Data.Bifunctor (bimap)
import Data.Default (Default(..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout
import Reflex.Vty.Widget.Input.Mouse

-- | Configuration options for a 'textInput'. For more information on
-- 'TextZipper', see 'Data.Text.Zipper'.
data TextInputConfig t e = TextInputConfig
  { _textInputConfig_initialValue :: TextZipper
  -- ^ Initial value. This is a 'TextZipper' because it is more flexible
  -- than plain 'Text'. For example, this allows to set the Cursor position,
  -- by choosing appropriate values for '_textZipper_before' and '_textZipper_after'.
  , _textInputConfig_modify :: Event t (TextZipper -> TextZipper)
  -- ^ Event to update the value of the 'textInput'.
  --
  -- Event is applied after other Input sources have been applied to the 'TextZipper',
  -- thus you may modify the final value that is displayed to the user.
  --
  -- You may set the value of the displayed text in 'textInput' by ignoring the input parameter.
  --
  -- Additionally, you can modify the updated value before displaying it to the user.
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
  , _textInputConfig_tabWidth :: Int
  , _textInputConfig_display :: Dynamic t (Char -> Char)
  -- ^ Transform the characters in a text input before displaying them. This is useful, e.g., for
  -- masking characters when entering passwords.
  , _textInputConfig_interpreter :: Int -> Int -> Maybe Text -> V.Event -> TextZipper -> Either e TextZipper
  -- ^ Interpret input edit events, optionally by refusing to modify the text and signalling and error.
  -- The interpreter takes:
  --  - the current tab width and page size,
  --  - the input event to be interpreted,
  --  - the currently possible completion, and
  --  - the state to be modified by the event.
  , _textInputConfig_completion :: Behavior t (Maybe Text)
  --  ^ An optional suitable completion, that the user can choose to insert,
  }

instance Reflex t => Default (TextInputConfig t e) where
  def = TextInputConfig empty never 4 (pure id) updateTextZipper (pure Nothing)

-- | The output produced by text input widgets, including the text
-- value and the number of display lines (post-wrapping). Note that some
-- display lines may not be visible due to scrolling.
-- The text value is accompanied by an optional error state,
-- as produced by the configured input event handler.
data TextInput t e = TextInput
  { _textInput_value :: Dynamic t (Maybe e, Text)
  -- ^ The current value of the textInput as Text, with an optional error status.
  , _textInput_userInput :: Event t (Maybe e, TextZipper)
  -- ^ UI Event updates with the current 'TextZipper'.
  -- This does not include Events added by '_textInputConfig_setValue', but
  -- it does include '_textInputConfig_modify' Events.
  , _textInput_lines :: Dynamic t Int
  , _textInput_position :: Dynamic t (Int, Int)
    -- ^ Current cursor row and column.
  }

-- | A widget that allows text input
textInput
  :: forall t m e.
     (Reflex t, MonadHold t m, MonadFix m, HasInput t m, HasFocusReader t m, HasTheme t m, HasDisplayRegion t m, HasImageWriter t m, HasDisplayRegion t m)
  => TextInputConfig t e
  -> m (TextInput t e)
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
      let valueChangedByCaller = _textInputConfig_modify cfg
      let valueChangedByKeys :: Event t ((Maybe e, TextZipper) -> (Maybe e, TextZipper))
          valueChangedByKeys =
            attach (_textInputConfig_completion cfg) (attach (current dh) i) <&>
              (\(curCompletion, (curDisplayHeight, inputE)) (_, old) ->
                 case (_textInputConfig_interpreter cfg) (_textInputConfig_tabWidth cfg) curDisplayHeight curCompletion inputE old of
                   Left err  -> (Just err, old)
                   Right new -> (Nothing, new))
      let valueChangeByClick :: Event t (TextZipper -> TextZipper)
          valueChangeByClick =
            let displayInfo = (,) <$> current rows <*> scrollTop
              in ffor (attach displayInfo click) $ \((dl, st), MouseDown _ (mx, my) _) ->
                goToDisplayLinePosition mx (st + my) dl
      let valueChangedByUI :: Event t ((Maybe e, TextZipper) -> (Maybe e, TextZipper))
          valueChangedByUI = mergeWith (.)
            [ valueChangedByKeys
            , valueChangeByClick <&> bimap (const Nothing) -- Clicks discard input errors, which should seem logical.
            ]
      let fullInitialState = (,) Nothing (_textInputConfig_initialValue cfg)
      v :: Dynamic t (Maybe e, TextZipper) <- foldDyn ($) fullInitialState $ mergeWith (.)
        [ valueChangedByCaller <&> bimap id -- Keep the interpreter-produced error state unaffected by the forced input changes.
                                            -- This is clearly suboptimal, but we need an API discussion to resolve the model issues.
        , valueChangedByUI
        ]
      click <- mouseDown V.BLeft

      -- TODO reverseVideo is prob not what we want. Does not work with `darkTheme` in example.hs (cursor is dark rather than light bg)
      let toCursorAttrs attr = V.withStyle attr V.reverseVideo
          rowInputDyn = (,,)
            <$> dw
            <*> (mapZipper <$> _textInputConfig_display cfg <*> fmap snd v)
            <*> f
          toDisplayLines attr (w, s, posx)  =
            let c = if posx then toCursorAttrs attr else attr
            in displayLines w attr c s
      attrDyn <- holdDyn attr0 $ pushAlways (\_ -> sample bt) (updated rowInputDyn)
      let rows = ffor2 attrDyn rowInputDyn toDisplayLines
          img = images . _displayLines_spans <$> rows
      x <- holdUniqDyn $ T.length . _textZipper_before . snd <$> v
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
    { _textInput_value = bimap id value <$> v
    , _textInput_userInput = attachWith (&) (current v) valueChangedByUI
    , _textInput_lines = length . _displayLines_spans <$> rows
    , _textInput_position = zipDyn x y
    }

-- | A widget that allows multiline text input
multilineTextInput
  :: (Reflex t, MonadHold t m, MonadFix m, HasInput t m, HasFocusReader t m, HasTheme t m, HasDisplayRegion t m, HasImageWriter t m)
  => TextInputConfig t e
  -> m (TextInput t e)
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
  :: (MonadFix m, MonadHold t m, HasLayout t m, HasInput t m, HasFocus t m, HasImageWriter t m, HasDisplayRegion t m, HasFocusReader t m, HasTheme t m)
  => m (TextInput t e)
  -> Dynamic t Int
  -> m (TextInput t e)
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
  -> Maybe Text -- ^ Completion
  -> V.Event -- ^ The vty event to handle
  -> TextZipper -- ^ The zipper to modify
  -> Either e TextZipper
updateTextZipper tabWidth pageSize _completion ev = case ev of
  -- Special characters
  V.EvKey (V.KChar '\t') [] -> Right . tab tabWidth
  -- Regular characters
  V.EvKey (V.KChar k) [] -> Right . insertChar k
  -- Deletion buttons
  V.EvKey V.KBS [] -> Right . deleteLeft
  V.EvKey V.KDel [] -> Right . deleteRight
  -- Key combinations
  V.EvKey (V.KChar 'u') [V.MCtrl] -> Right . const empty
  V.EvKey (V.KChar 'w') [V.MCtrl] -> Right . deleteLeftWord
  -- Arrow keys
  V.EvKey V.KLeft [] -> Right . left
  V.EvKey V.KRight [] -> Right . right
  V.EvKey V.KUp [] -> Right . up
  V.EvKey V.KDown [] -> Right . down
  V.EvKey V.KHome [] -> Right . home
  V.EvKey V.KEnd [] -> Right . end
  V.EvKey V.KPageUp [] -> Right . pageUp pageSize
  V.EvKey V.KPageDown [] -> Right . pageDown pageSize
  _ -> Right . id
