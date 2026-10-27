{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Scoped overlay primitives: modal, dialog, and popover.
--
-- These combinators own both the background and foreground boundary so
-- they can isolate input, focus and cursor writes. A combinator
-- accepting only a foreground child cannot suppress arbitrary
-- ancestor/sibling handlers; do not claim otherwise.
--
-- Nesting contract: connect '_overlayConfig_childActive' to the
-- directly nested foreground overlay's '_overlay_visible' (use Reflex
-- 'rec' when the child result is returned by the parent). Pre-frame
-- sampling means the Escape that closes an inner overlay cannot also
-- dismiss its parent.
module Reflex.Vty.Widget.Overlay
  ( OverlayChild
  , OverlayPosition (..)
  , OverlayBackdrop (..)
  , OverlayDismiss (..)
  , OverlayConfig (..)
  , Overlay (..)
  , modal
  , dialog
  , popover
  , defaultPopoverConfig
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Default (Default (..))
import Data.Semigroup (Last (..))
import Data.Text (Text)
import Data.Text.Zipper (TextAlignment (..))
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as V
import Reflex

import qualified Reflex.Vty as Core
import Reflex.Vty (Region (..), MouseTrackingState (..))
import Reflex.Vty.Canvas
  ( Canvas
  , blankCanvas
  , canvasToImage
  , placeCanvas
  )
import Reflex.Vty.Style (Style, applyAttr)
import Reflex.Vty.Widget.Box (boxTitle, singleBoxStyle)
import Reflex.Vty.Widget.Key
  ( KeyBinding (..)
  , KeyMap (..)
  , dispatchTable
  )
import qualified Reflex.Vty.Widget.Layout as L

-- | The type of overlay layers: a layout action with its own focus
-- manager, running against the caller's monad.
type OverlayChild t m a =
  L.Layout
    t
    (L.Focus
      t
      (Core.Input t (Core.CursorWriter t m)))
    a

data OverlayPosition
  = OverlayCentered
  | OverlayAt Int Int
  | OverlayBelow Region
  deriving (Eq, Show)

data OverlayBackdrop
  = NoBackdrop
  | SolidBackdrop Style
  | CanvasBackdrop Canvas

data OverlayDismiss = OverlayDismissKey | OverlayDismissOutside
  deriving (Eq, Show)

data OverlayConfig t = OverlayConfig
  { _overlayConfig_size :: Dynamic t (Int, Int)
  , _overlayConfig_position :: Dynamic t OverlayPosition
  , _overlayConfig_backdrop :: Behavior t OverlayBackdrop
  , _overlayConfig_panelStyle :: Behavior t (Maybe Style)
  , _overlayConfig_dismissKeys :: Dynamic t (KeyMap ())
  , _overlayConfig_dismissOnOutsideClick :: Bool
  , _overlayConfig_childActive :: Dynamic t Bool
  }

instance Reflex t => Default (OverlayConfig t) where
  def =
    OverlayConfig
      { _overlayConfig_size = pure (40, 10)
      , _overlayConfig_position = pure OverlayCentered
      , _overlayConfig_backdrop = pure NoBackdrop
      , _overlayConfig_panelStyle = pure (Just def)
      , _overlayConfig_dismissKeys =
          pure
            ( KeyMap
                [ KeyBinding
                    { _keyBinding_action = ()
                    , _keyBinding_keys = [(V.KEsc, [])]
                    , _keyBinding_help = "close"
                    , _keyBinding_enabled = True
                    }
                ]
            )
      , _overlayConfig_dismissOnOutsideClick = False
      , _overlayConfig_childActive = pure False
      }

data Overlay t a b = Overlay
  { _overlay_background :: a
  , _overlay_foreground :: b
  , _overlay_visible :: Dynamic t Bool
  , _overlay_dismissed :: Event t OverlayDismiss
  }

-- | A popover is 'def' with outside-click dismissal enabled.
defaultPopoverConfig :: Reflex t => OverlayConfig t
defaultPopoverConfig =
  def {_overlayConfig_dismissOnOutsideClick = True}

-- | Centered modal: captures the whole screen behind a fitted panel.
modal
  :: forall t m a b
   . ( Reflex t, MonadFix m, MonadHold t m, Core.MonadNodeId m
     , PostBuild t m, Core.HasInput t m, Core.HasFocusReader t m
     , Core.HasDisplayRegion t m, Core.HasImageWriter t m, Core.HasTheme t m
     , Core.HasCursor t m
     )
  => OverlayConfig t
  -> Dynamic t Bool
  -> OverlayChild t m a
  -> OverlayChild t m b
  -> m (Overlay t a b)
modal = runOverlay Nothing

-- | A titled dialog: same isolation as 'modal' with a drawn frame. The
-- real body runs through a pane inset by one cell on each side of the
-- frame, keeping the body mounted once even at tiny sizes.
dialog
  :: forall t m a b
   . ( Reflex t, MonadFix m, MonadHold t m, Core.MonadNodeId m
     , PostBuild t m, Core.HasInput t m, Core.HasFocusReader t m
     , Core.HasDisplayRegion t m, Core.HasImageWriter t m, Core.HasTheme t m
     , Core.HasCursor t m
     )
  => OverlayConfig t
  -> Behavior t Text
  -> Dynamic t Bool
  -> OverlayChild t m a
  -> OverlayChild t m b
  -> m (Overlay t a b)
dialog cfg title = runOverlay (Just title) cfg

-- | An anchored popover: positioned below a live anchor region,
-- focus-taking while visible; configure outside-click dismissal via
-- 'defaultPopoverConfig'.
popover
  :: forall t m a b
   . ( Reflex t, MonadFix m, MonadHold t m, Core.MonadNodeId m
     , PostBuild t m, Core.HasInput t m, Core.HasFocusReader t m
     , Core.HasDisplayRegion t m, Core.HasImageWriter t m, Core.HasTheme t m
     , Core.HasCursor t m
     )
  => OverlayConfig t
  -> Dynamic t Region
  -> Dynamic t Bool
  -> OverlayChild t m a
  -> OverlayChild t m b
  -> m (Overlay t a b)
popover cfg anchor req bg fg =
  runOverlay Nothing cfg {_overlayConfig_position = OverlayBelow <$> anchor} req bg fg

-- | One overlay boundary: both layers mounted exactly once, each with
-- its own focus/layout manager; visibility deferred across in-progress
-- mouse gestures; dismiss detection at the boundary.
runOverlay
  :: forall t m a b
   . ( Reflex t, MonadFix m, MonadHold t m, Core.MonadNodeId m
     , PostBuild t m, Core.HasInput t m, Core.HasFocusReader t m
     , Core.HasDisplayRegion t m, Core.HasImageWriter t m, Core.HasTheme t m
     , Core.HasCursor t m
     )
  => Maybe (Behavior t Text)
  -- ^ Dialog title (Nothing for plain modal/popover).
  -> OverlayConfig t
  -> Dynamic t Bool
  -- ^ Requested visibility.
  -> OverlayChild t m a
  -> OverlayChild t m b
  -> m (Overlay t a b)
runOverlay mTitle cfg requestedD bgChild fgChild = do
  vp <- Core.askViewport
  inp <- Core.input
  pb <- getPostBuild
  ambFocD <- Core.focus
  let fgRectD = fitRect <$> vp <*> _overlayConfig_size cfg <*> _overlayConfig_position cfg
      bodyRectD = ffor fgRectD $ \(Region _ _ w h) -> case mTitle of
        Nothing -> Region 0 0 w h
        Just _ -> Region (min 1 w) (min 1 h) (max 0 (w - 2)) (max 0 (h - 2))
      bgAreaD = hasArea <$> vp
      fgAreaD = hasArea <$> bodyRectD

  -- The first ordinary button owns the gesture. Neither wheel activity
  -- nor another button's release can settle it.
  let stepGesture e old = case (old, e) of
        (WaitingForInput, V.EvMouseDown _ _ btn _)
          | btn /= V.BScrollUp && btn /= V.BScrollDown -> Just (Tracking btn)
        (Tracking btn, V.EvMouseUp _ _ released)
          | released == Nothing || released == Just btn -> Just WaitingForInput
        _ -> Nothing
  gestureD <- foldDynMaybe stepGesture WaitingForInput inp
  initialVisible <- sample (current requestedD)
  visibleD <- foldDynMaybe
    (\(requested, gesture) old ->
      if gesture == WaitingForInput && requested /= old then Just requested else Nothing)
    initialVisible
    (updated (zipDyn requestedD gestureD))

  -- All boundary decisions use pre-frame values. An opener or closing
  -- release belongs to the old layer, never the newly active one.
  let eligible = (&&) <$> current visibleD
        <*> ((&&) <$> (not <$> current (_overlayConfig_childActive cfg)) <*> current ambFocD)
      dismissKeyE = attachWithMaybe
        (\(ok, tbl) e -> case e of
          V.EvKey k mods | ok -> OverlayDismissKey <$ Map.lookup (k, mods) tbl
          _ -> Nothing)
        ((,) <$> eligible <*> current (dispatchTable <$> _overlayConfig_dismissKeys cfg))
        inp
      outsideE
        | _overlayConfig_dismissOnOutsideClick cfg = attachWithMaybe
            (\(ok, r) e -> case e of
              V.EvMouseDown x y V.BLeft _
                | ok && not (Core.withinRegion r x y) -> Just OverlayDismissOutside
              _ -> Nothing)
            ((,) <$> eligible <*> current fgRectD)
            inp
        | otherwise = never
      dismissed = leftmost [dismissKeyE, outsideE]
      routeInput active e = case e of
        V.EvResize {} -> Just e
        V.EvGainedFocus -> Just e
        V.EvLostFocus -> Just e
        _ -> if active then Just e else Nothing
      bgRoute = attachWithMaybe (routeInput . not) (current visibleD) inp
      fgRoute = difference (attachWithMaybe routeInput (current visibleD) inp) dismissed
      bgActiveD = (&&) <$> (not <$> visibleD) <*> bgAreaD
      fgActiveD = (&&) <$> visibleD <*> fgAreaD
  (bgResult, bgImages, bgCursor) <-
    Core.localFocus (zipDynWith (&&) bgActiveD) $
      runLayer bgRoute (gateArea bgAreaD bgChild)
  (fgResult, fgImages, fgCursor) <-
    Core.localFocus (zipDynWith (&&) fgActiveD) $
      runLayer fgRoute $
        Core.pane fgRectD (pure True) $ do
          case mTitle of
            Nothing -> Core.blank
            Just title -> do
              let frameArea = (\r -> _region_width r >= 2 && _region_height r >= 2) <$> fgRectD
              Core.mapImages (liftA2 (\ok images -> if ok then images else []) (current frameArea)) $
                boxTitle (pure TextAlignment_Center) (pure singleBoxStyle) title Core.blank
          Core.pane bodyRectD (pure True) $ gateArea fgAreaD fgChild

  -- Capture happens inside runLayer, outside the panel pane, so images
  -- have their local panel translation and cursors are already absolute.
  Core.tellImages $
    (\vis viewport rect bgIm fgIm backdrop panel ->
      if not vis then bgIm else
        bgIm ++ backdropImages (_region_width viewport) (_region_height viewport) backdrop
          ++ panelImages rect panel ++ fgIm)
      <$> current visibleD <*> current vp <*> current fgRectD
      <*> bgImages <*> fgImages
      <*> _overlayConfig_backdrop cfg <*> _overlayConfig_panelStyle cfg

  bgCursorD <- holdDyn Core.defaultCursorState (getLast <$> bgCursor)
  fgCursorD <- holdDyn Core.defaultCursorState (getLast <$> fgCursor)
  let activeAreaD = zipDynWith (\vis (bg, fg) -> if vis then fg else bg)
        visibleD (zipDyn bgAreaD fgAreaD)
      selectedD =
        (\vis area bg fg ->
          let selected = if vis then fg else bg
          in if area then selected else selected { Core._cursorState_visibility = Core.CursorHidden })
          <$> visibleD <*> activeAreaD <*> bgCursorD <*> fgCursorD
      cursorTrigger = leftmost
        [ pb
        , () <$ updated visibleD
        , () <$ updated activeAreaD
        , () <$ gate (current (not <$> visibleD)) bgCursor
        , () <$ gate (current visibleD) fgCursor
        ]
  Core.tellCursor $ tagPromptlyDyn selectedD cursorTrigger
  pure Overlay
    { _overlay_background = bgResult
    , _overlay_foreground = fgResult
    , _overlay_visible = visibleD
    , _overlay_dismissed = dismissed
    }
  where
    hasArea r = _region_width r > 0 && _region_height r > 0

    -- Retain notifications and completion of captured gestures even when
    -- the drawable body has shrunk to zero. Keep the child mounted.
    gateArea areaD child = do
      events <- Core.input
      let gated = attachWithMaybe
            (\area e -> case e of
              V.EvKey {} -> if area then Just e else Nothing
              V.EvPaste {} -> if area then Just e else Nothing
              V.EvMouseDown {} -> if area then Just e else Nothing
              _ -> Just e)
            (current areaD) events
      Core.localInput (const gated) child

    backdropImages vw vh bd = case bd of
      NoBackdrop -> []
      SolidBackdrop st ->
        [V.charFill (applyAttr st V.defAttr) ' ' (max 0 vw) (max 0 vh)]
      CanvasBackdrop c ->
        [canvasToImage (placeCanvas 0 0 c (blankCanvas (max 0 vw) (max 0 vh)))]

    panelImages _ Nothing = []
    panelImages rect (Just style)
      | hasArea rect =
          [Core.withinImage rect $
            V.charFill (applyAttr style V.defAttr) ' ' (_region_width rect) (_region_height rect)]
      | otherwise = []

-- | Fitted coordinates are local to the caller; pane applies its origin.
fitRect :: Region -> (Int, Int) -> OverlayPosition -> Region
fitRect viewport (rw, rh) position =
  let vw = max 0 (_region_width viewport)
      vh = max 0 (_region_height viewport)
      w = max 0 (min vw rw)
      h = max 0 (min vh rh)
      slackW = vw - w
      slackH = vh - h
      clamp hi = max 0 . min hi
      (x, y) = case position of
        OverlayCentered -> (slackW `div` 2, slackH `div` 2)
        OverlayAt px py -> (px, py)
        OverlayBelow anchor ->
          let below = _region_top anchor + _region_height anchor
          in (_region_left anchor, if below + h <= vh then below else _region_top anchor - h)
  in Region (clamp slackW x) (clamp slackH y) w h

-- | Run one overlay layer exactly once: a separate 'Core.Input' with
-- its own focus/layout manager (including 'tabNavigation'), capturing
-- its images and cursor requests without emitting them directly.
runLayer
  :: forall t m a
   . ( Reflex t, MonadFix m, MonadHold t m, Core.MonadNodeId m
     , Core.HasInput t m, Core.HasFocusReader t m, Core.HasDisplayRegion t m
     , Core.HasImageWriter t m, Core.HasCursor t m
     )
  => Event t V.Event
  -> OverlayChild t m a
  -> m (a, Behavior t [V.Image], Event t (Last Core.CursorState))
runLayer layerInput child = do
  -- Capture the layer's images (and cursor) so the boundary alone
  -- decides what is emitted. Stack: Input -> CursorWriter -> Focus ->
  -- Layout, with the layer's own focus manager including
  -- tabNavigation.
  (r, images) <- Core.captureImages $ do
    (a, cursorE) <- Core.runCursorWriter $ do
      (a2, _focusIds) <- Core.runInput layerInput $ L.initManager $ do
        L.tabNavigation
        child
      pure a2
    pure (a, cursorE)
  pure (fst r, images, snd r)
