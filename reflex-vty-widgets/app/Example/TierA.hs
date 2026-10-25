{-# LANGUAGE PackageImports #-}

-- | Interactive Tier A demonstration: remappable keys with generated
-- help, a state-preserving grid, a paginator, and a dialog\/popover.
-- All background controls are routed through one 'withKeyMap' at the
-- root so dialog text entry never sees them; @q@ exits only where the
-- background layer owns input, and Ctrl+C exits from any state.
module Example.TierA (runDemo, demoWidget) where

import Control.Monad.Fix (MonadFix)
import Data.Functor (void)
import qualified Data.Text as T
import Data.Text.Zipper (fromText)
import qualified Graphics.Vty as V
import Reflex

import "reflex-vty" Reflex.Vty

import "reflex-vty-widgets" Reflex.Vty.Widget

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
        [False <$ _overlay_dismissed outer, True <$ command CmdOpenDialog rootActions]
      outer <- dialog
        (def { _overlayConfig_size = pure (52,12)
             , _overlayConfig_position = pure OverlayCentered
             , _overlayConfig_backdrop = pure (SolidBackdrop def)
             , _overlayConfig_childActive = nestedVisible })
        (pure "Tier A dialog") requested
        (do
          rec remapped <- foldDyn (const not) False (command CmdRemap actions)
              enabled <- foldDyn (const not) True (command CmdToggleEnabled actions)
              increments <- count (command CmdIncrement actions)
              let liveMap = rootKeyMap <$> remapped <*> enabled
              (actions, ()) <- withKeyMap liveMap $ col $ do
                grout (fixed (pure 1)) $
                  statusRow increments ((\r -> if r then 'i' else 'x') <$> remapped) enabled
                grout (fixed (pure 7)) $ void $
                  rowWithSpacing (def { _spacingConfig_gap = pure 2 })
                    [ grout flex $ keyHelp (KeyMap . take 7 . _keyMap_bindings <$> liveMap)
                    , grout flex $ keyHelp (KeyMap . drop 7 . _keyMap_bindings <$> liveMap)
                    ]
                grout flex $ gridSection actions
                grout (fixed (pure 1)) $ paginationRow actions
                grout (fixed (pure 2)) $
                  text (pure "Tab/Shift+Tab: field focus | m: dialog\nq: quit background only | Ctrl+C: quit anywhere")
          pure actions)
        dialogBody
      let rootActions = _overlay_background outer
          nestedVisible = _overlay_foreground outer
  pure $ leftmost [ctrlC, command CmdQuit rootActions]

command :: Reflex t => RootCmd -> Event t RootCmd -> Event t ()
command cmd = void . ffilter (== cmd)

dialogBody
  :: ( Reflex t, MonadFix m, MonadHold t m, MonadNodeId m, PostBuild t m
     , HasInput t m, HasFocusReader t m, HasDisplayRegion t m
     , HasImageWriter t m, HasTheme t m, HasCursor t m
     )
  => m (Dynamic t Bool)
dialogBody = do
  rec requested <- holdDyn False $ leftmost
        [False <$ _overlay_dismissed popup, True <$ openPopup]
      popup <- popover
        (defaultPopoverConfig { _overlayConfig_size = pure (28,4) })
        anchor requested
        (withKeyMap
          (pure (KeyMap [KeyBinding () [(V.KChar 'p', [])] "open popover" True]))
          dialogForm)
        (grout flex $ text (pure "Nested popover\nEscape closes this popup"))
      let (openPopup, anchor) = _overlay_background popup
  pure (_overlay_visible popup)

dialogForm
  :: ( Reflex t, MonadFix m, MonadHold t m, PostBuild t m
     , HasInput t m, HasFocusReader t m, HasDisplayRegion t m
     , HasImageWriter t m, HasTheme t m, HasFocus t m, HasLayout t m
     )
  => m (Dynamic t Region)
dialogForm = col $ do
  (firstId, _) <- axis (pure Orientation_Row) (fixed (pure 1)) $ do
    grout (fixed (pure 8)) $ text (pure "First:")
    tile' flex (textInput def)
  void $ axis (pure Orientation_Row) (fixed (pure 1)) $ do
    grout (fixed (pure 8)) $ text (pure "Second:")
    tile' flex (textInput def)
  pb <- getPostBuild
  requestFocus (Refocus_Id firstId <$ pb)
  anchor <- region (fixed (pure 1))
  pane anchor (pure True) $ text (pure "p: open popover")
  grout (fixed (pure 1)) $ text (pure "Tab/Shift+Tab: switch field")
  grout (fixed (pure 1)) $ text (pure "Escape: close dialog")
  pure anchor

data RootCmd
  = CmdIncrement
  | CmdRemap
  | CmdToggleEnabled
  | CmdColumns
  | CmdGapUp
  | CmdGapDown
  | CmdPadding
  | CmdPagePrev
  | CmdPageNext
  | CmdDisplay
  | CmdTotal
  | CmdOpenDialog
  | CmdQuit
  deriving (Eq, Show)

rootKeyMap :: Bool -> Bool -> KeyMap RootCmd
rootKeyMap remapped enabled =
  setBindingEnabled CmdIncrement enabled $ KeyMap
    [ KeyBinding CmdIncrement [(V.KChar (if remapped then 'i' else 'x'), [])] "increment counter" True
    , KeyBinding CmdRemap [(V.KChar 'r', [])] "remap increment x/i" True
    , KeyBinding CmdToggleEnabled [(V.KChar 'd', [])] "enable/disable increment" True
    , KeyBinding CmdColumns [(V.KFun 6, [])] "toggle columns" True
    , KeyBinding CmdGapUp [(V.KChar '+', [])] "increase gap" True
    , KeyBinding CmdGapDown [(V.KChar '-', [])] "decrease gap" True
    , KeyBinding CmdPadding [(V.KFun 7, [])] "toggle padding" True
    , KeyBinding CmdPagePrev [(V.KLeft, [V.MCtrl])] "previous page" True
    , KeyBinding CmdPageNext [(V.KRight, [V.MCtrl])] "next page" True
    , KeyBinding CmdDisplay [(V.KFun 8, [])] "dots/numeric" True
    , KeyBinding CmdTotal [(V.KFun 9, [])] "cycle total" True
    , KeyBinding CmdOpenDialog [(V.KChar 'm', [])] "open dialog" True
    , KeyBinding CmdQuit [(V.KChar 'q', [])] "quit" True
    ]

statusRow
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => Dynamic t Int -> Dynamic t Char -> Dynamic t Bool -> m ()
statusRow countD actionD enabledD =
  text
    ( ( \c a e ->
          T.pack
            ( "count "
                ++ show (c :: Int)
                ++ " | increment key '"
                ++ [a]
                ++ "' "
                ++ (if e then "enabled" else "DISABLED")
            )
      )
        <$> current countD
        <*> current actionD
        <*> current enabledD
    )

gridSection
  :: ( Reflex t, MonadHold t m, MonadFix m, MonadNodeId m, PostBuild t m
     , HasInput t m, HasImageWriter t m, HasDisplayRegion t m
     , HasFocusReader t m, HasTheme t m, HasFocus t m, HasLayout t m
     )
  => Event t RootCmd
  -> m ()
gridSection cmdE = do
  let colsE = fforMaybe cmdE $ \case
        CmdColumns -> Just ()
        _ -> Nothing
      gapUpE = fforMaybe cmdE $ \case
        CmdGapUp -> Just ()
        _ -> Nothing
      gapDownE = fforMaybe cmdE $ \case
        CmdGapDown -> Just ()
        _ -> Nothing
      padE = fforMaybe cmdE $ \case
        CmdPadding -> Just ()
        _ -> Nothing
  colsD <- foldDyn (const (\c -> if c == 3 then 2 else 3)) 3 colsE
  gapD <-
    foldDyn
      (\up g -> if up then g + 1 else max 0 (g - 1))
      1
      (leftmost [True <$ gapUpE, False <$ gapDownE])
  padD <- foldDyn (const (\p -> if p == 1 then 0 else 1)) 1 padE
  let cfg =
        GridConfig
          { _gridConfig_columns = colsD
          , _gridConfig_columnGap = gapD
          , _gridConfig_rowGap = gapD
          , _gridConfig_padding = (\p -> Padding p p p p) <$> padD
          }
  results <-
    grid cfg $
      [ tile' flex (textInput def {_textInputConfig_initialValue = fromText (T.pack label)})
      | label <- ["A", "B", "C", "D", "E", "F"]
      ]
  pb <- getPostBuild
  case results of
    (firstId, _) : _ -> requestFocus (Refocus_Id firstId <$ pb)
    [] -> pure ()

paginationRow
  :: ( Reflex t, MonadHold t m, MonadFix m
     , HasInput t m, HasDisplayRegion t m, HasImageWriter t m, HasTheme t m
     )
  => Event t RootCmd
  -> m ()
paginationRow cmdE = do
  let dispE = fforMaybe cmdE $ \case
        CmdDisplay -> Just ()
        _ -> Nothing
      totalE = fforMaybe cmdE $ \case
        CmdTotal -> Just ()
        _ -> Nothing
  dispD <- foldDyn (const toggleDisplay) PaginatorDots dispE
  totalD <- foldDyn (const cycleTotal) 23 totalE
  -- Apply the pre-frame command through movePage and feed the resulting
  -- index back as an explicit set request.
  let navCommandE = fforMaybe cmdE $ \case
        CmdPagePrev -> Just PagePrevious
        CmdPageNext -> Just PageNext
        _ -> Nothing
  rec stateD <- paginator
        def
          { _paginatorConfig_totalItems = totalD
          , _paginatorConfig_pageSize = pure 5
          , _paginatorConfig_keys = pure (KeyMap [])
          , _paginatorConfig_setPage =
              attachWith (\st cmd -> pageIndex (movePage cmd st)) (current stateD) navCommandE
          , _paginatorConfig_display = current dispD
          , _paginatorConfig_initialPage = 0
          }
  pure ()
  where
    toggleDisplay d = case d of
      PaginatorDots -> PaginatorNumeric
      PaginatorNumeric -> PaginatorDots
    cycleTotal t = case t of
      23 -> 3
      3 -> 0
      _ -> 23

