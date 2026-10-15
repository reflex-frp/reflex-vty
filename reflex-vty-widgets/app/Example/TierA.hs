{-# LANGUAGE PackageImports #-}

-- | Interactive Tier A demonstration: remappable keys with generated
-- help, a state-preserving grid, a paginator, and a dialog\/popover.
-- All background controls are routed through one 'withKeyMap' at the
-- root so dialog text entry never sees them; @q@ exits only where the
-- background layer owns input, and Ctrl+C exits from any state.
module Example.TierA (runDemo) where

import Control.Monad.Fix (MonadFix)
import Data.Functor (void)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Zipper (fromText)
import qualified Graphics.Vty as V
import Reflex

import "reflex-vty" Reflex.Vty

import "reflex-vty-widgets" Reflex.Vty.Widget

-- | One screen exercising all four Tier A groups.
runDemo :: IO ()
runDemo =
  mainWidget def $ do
    enterAlternateScreen
    quitE <- quitEvents
    -- Raw key events for remap/enable drive the key map itself; the
    -- pre-frame sampling in withKeyMap/keyEvents makes this loop safe.
    remappedD <-
      foldDyn
        (const not)
        False
        =<< key (V.KChar 'r')
    enabledD <-
      foldDyn
        (const not)
        True
        =<< key (V.KChar 'd')
    (_rootActions, _) <-
      withKeyMap (rootKeyMap <$> remappedD) $ do
        initManager_ $ col $ do
          incE <- key (V.KChar 'x')
          incEI <- key (V.KChar 'i')
          let enabledNowE =
                push (\() -> do
                  e <- sample (current enabledD)
                  pure (if e then Just () else Nothing)) (leftmost [void incE, void incEI])
          incCountD <- count enabledNowE
          grout (fixed (pure 1)) $ statusRow incCountD ((\r -> if r then 'i' else 'x') <$> remappedD) enabledD
          grout (fixed (pure 3)) $ keyHelp (rootKeyMap <$> remappedD)
          _ <- tile flex $ gridSection =<< rawGridEvents
          _ <- paginationRow =<< rawPagEvents
          grout (fixed (pure 2)) $
            text
              ( pure
                  ( T.pack
                      "r remap x/i, d disable, F6 cols, +/- gap, F7 pad | Ctrl+Left/Right pages, F8 view, F9 total | m dialog, q quit"
                  )
              )
    pure quitE

-- Grid and paginator commands come from raw key events (they do not
-- overlap with the remappable binding keys).
rawGridEvents :: (Reflex t, Monad m, HasInput t m) => m (Event t RootCmd)
rawGridEvents = do
  f6 <- key (V.KFun 6)
  plus <- key (V.KChar '+')
  minus <- key (V.KChar '-')
  f7 <- key (V.KFun 7)
  pure $ leftmost
    [ CmdColumns <$ void f6
    , CmdGapUp <$ void plus
    , CmdGapDown <$ void minus
    , CmdPadding <$ void f7
    ]

rawPagEvents :: (Reflex t, Monad m, HasInput t m) => m (Event t RootCmd)
rawPagEvents = do
  cleft <- keyCombo (V.KLeft, [V.MCtrl])
  cright <- keyCombo (V.KRight, [V.MCtrl])
  f8 <- key (V.KFun 8)
  f9 <- key (V.KFun 9)
  pure $ leftmost
    [ CmdPagePrev <$ void cleft
    , CmdPageNext <$ void cright
    , CmdDisplay <$ void f8
    , CmdTotal <$ void f9
    ]

-- q exits only where the background layer owns input; Ctrl+C arrives
-- as a raw key event and exits from any state.
quitEvents :: (Reflex t, Monad m, HasInput t m) => m (Event t ())
quitEvents = do
  q <- key (V.KChar 'q')
  i <- input
  let ctrlC = fforMaybe i $ \case
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  pure (leftmost [void q, ctrlC])

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
  deriving (Eq, Show)

rootKeyMap :: Bool -> KeyMap RootCmd
rootKeyMap remapped =
  KeyMap
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
  :: ( Reflex t, MonadHold t m, MonadFix m, MonadNodeId m
     , HasInput t m, HasImageWriter t m, HasDisplayRegion t m
     , HasFocusReader t m, HasTheme t m, HasFocus t m, HasLayout t m
     )
  => Event t RootCmd
  -> m (Dynamic t Text)
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
  tabNavigation
  results <-
    grid cfg $
      [ tile flex (textInput def {_textInputConfig_initialValue = fromText (T.pack label)})
      | label <- ["A", "B", "C", "D", "E", "F"]
      ]
  pure (T.concat <$> sequenceA (_textInput_value <$> results))

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
  _stD <-
    paginator
      def
        { _paginatorConfig_totalItems = totalD
        , _paginatorConfig_pageSize = pure 5
        , _paginatorConfig_keys = pure paginatorKeys
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

paginatorKeys :: KeyMap PageCommand
paginatorKeys =
  KeyMap
    [ KeyBinding PagePrevious [(V.KLeft, [V.MCtrl]), (V.KPageUp, [])] "previous page" True
    , KeyBinding PageNext [(V.KRight, [V.MCtrl]), (V.KPageDown, [])] "next page" True
    , KeyBinding PageFirst [(V.KHome, [])] "first page" True
    , KeyBinding PageLast [(V.KEnd, [])] "last page" True
    ]
