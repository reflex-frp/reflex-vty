{-# LANGUAGE PackageImports #-}

-- | A minimal note editor: one committed note on the background and one
-- single-field dialog for editing a draft.
--
-- @e@ opens the dialog and @q@ quits, but only from the background. The
-- dialog owns input while it is visible, so @e@, @q@, and every other
-- printable character are ordinary text there. Ctrl+C quits from any
-- state. Escape discards the draft; Enter commits it. Every open resets
-- the draft from the committed note with the cursor at the end.
module Example.TierA (runDemo, demoWidget) where

import Control.Monad.Fix (MonadFix)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper (end, fromText, textWidth)
import qualified Graphics.Vty as V
import Reflex

import "reflex-vty" Reflex.Vty
import "reflex-vty-widgets" Reflex.Vty.Widget

-- | The note shown before the user commits an edit.
initialNote :: Text
initialNote = "Ship on Friday"

-- | The executable and regression harness share the same widget.
runDemo :: IO ()
runDemo = mainWidget def demoWidget

demoWidget
  :: ( MonadVtyApp t m, MonadNodeId m, HasImageWriter t m
     , HasDisplayRegion t m, HasFocusReader t m, HasInput t m
     , HasTheme t m, HasColorProfile t m, HasCursor t m
     , HasScreenMode t m
     )
  => m (Event t ())
demoWidget = do
  enterAlternateScreen
  events <- input
  let ctrlC = fforMaybe events $ \case
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  rec requested <- holdDyn False $ leftmost
        [ False <$ _overlay_dismissed outer
        , True <$ openE
        , False <$ commitE
        ]
      committedD <- holdDyn initialNote commitE
      let openE = command CmdEdit backgroundActions
      outer <-
        dialog
          ( def
              { _overlayConfig_size = pure (40, 5)
              , _overlayConfig_position = pure OverlayCentered
              , _overlayConfig_backdrop = pure (SolidBackdrop def)
              }
          )
          (pure "Edit note")
          requested
          (noteBackground committedD)
          (noteDialog committedD openE)
      let backgroundActions = _overlay_background outer
          commitE = _overlay_foreground outer
  pure $ leftmost [ctrlC, command CmdQuit backgroundActions]

-- | The background owns @e@ (open) and @q@ (quit).
data NoteCmd
  = CmdEdit
  | CmdQuit
  deriving (Eq, Show)

noteKeyMap :: KeyMap NoteCmd
noteKeyMap =
  KeyMap
    [ KeyBinding CmdEdit [(V.KChar 'e', [])] "edit" True
    , KeyBinding CmdQuit [(V.KChar 'q', [])] "quit" True
    ]

command :: Reflex t => NoteCmd -> Event t NoteCmd -> Event t ()
command cmd = void . ffilter (== cmd)

-- | Center the committed note and the two background controls vertically
-- with equal slack above and below, and horizontally in the viewport.
noteBackground
  :: ( Reflex t, MonadFix m, MonadHold t m
     , HasInput t m, HasFocusReader t m, HasDisplayRegion t m
     , HasImageWriter t m, HasTheme t m, HasLayout t m
     )
  => Dynamic t Text
  -> m (Event t NoteCmd)
noteBackground committedD = do
  (actions, ()) <-
    withKeyMap (pure noteKeyMap) $
      col $ do
        grout flex blank
        grout (fixed (pure 1)) $
          centeredLine (("Note: " <>) <$> current committedD)
        grout (fixed (pure 1)) $
          centeredLine (pure "e: Edit | q: Quit")
        grout flex blank
  pure actions

-- | Center one line by padding its left edge. @textWithAlignment@ currently
-- renders the wrapped text without its alignment offset, so the demo pads
-- explicitly: floor the slack on the left, leaving the remainder blank on
-- the right. This centers at any width without touching the text primitive.
centeredLine
  :: ( Reflex t, Monad m, HasDisplayRegion t m
     , HasImageWriter t m, HasTheme t m
     )
  => Behavior t Text
  -> m ()
centeredLine value = do
  width <- displayWidth
  text $
    ( \w s ->
        T.replicate (max 0 ((w - textWidth s) `div` 2)) " " <> s
    )
      <$> current width
      <*> value

-- | One mounted, single-field dialog. Escape is the overlay's dismissal
-- key, so it closes the dialog and discards the draft. Enter commits the
-- current field value and closes the dialog. Opening resets the draft from
-- the committed note and puts the cursor at the end.
noteDialog
  :: ( Reflex t, MonadFix m, MonadHold t m, PostBuild t m
     , HasInput t m, HasFocusReader t m, HasDisplayRegion t m
     , HasImageWriter t m, HasTheme t m, HasFocus t m, HasLayout t m
     )
  => Dynamic t Text
  -> Event t ()
  -> m (Event t Text)
noteDialog committedD openE = do
  let resetE =
        attachWith
          (\committed () -> const (end (fromText committed)))
          (current committedD)
          openE
  i <- input
  let enterE = () <$ ffilter isEnter i
  (fieldId, inputValue) <- col $ do
    (fid, ti) <-
      axis (pure Orientation_Row) (fixed (pure 1)) $
        tile' flex $
          textInput
            def
              { _textInputConfig_initialValue = fromText initialNote
              , _textInputConfig_modify = resetE
              }
    grout (fixed (pure 1)) $
      text (pure "Enter: Save | Escape: Cancel")
    grout flex blank
    pure (fid, _textInput_value ti)
  postBuild <- getPostBuild
  requestFocus $ Refocus_Id fieldId <$ postBuild
  pure $ attachWith (\value () -> value) (current inputValue) enterE
  where
    isEnter = \case
      V.EvKey V.KEnter [] -> True
      _ -> False
