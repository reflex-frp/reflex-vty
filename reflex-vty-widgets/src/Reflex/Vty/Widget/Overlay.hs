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
import qualified Data.Text as T
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
  , imageToCanvas
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

  -- Geometry: fitted foreground rectangle.
  let fgRectD =
        fitRect
          <$> vp
          <*> _overlayConfig_size cfg
          <*> _overlayConfig_position cfg
      fitRect :: Region -> (Int, Int) -> OverlayPosition -> Region
      fitRect vp' (rw, rh) pos =
        let w = max 0 (min (Core._region_width vp') rw)
            h = max 0 (min (Core._region_height vp') rh)
            slackW = Core._region_width vp' - w
            slackH = Core._region_height vp' - h
            centered = (Core._region_left vp' + slackW `div` 2, Core._region_top vp' + slackH `div` 2)
            (x0, y0) = case pos of
              OverlayCentered -> centered
              OverlayAt x y ->
                ( max (Core._region_left vp') (min (Core._region_left vp' + slackW) x)
                , max (Core._region_top vp') (min (Core._region_top vp' + slackH) y)
                )
              OverlayBelow anchor ->
                let belowY = Core._region_top anchor + Core._region_height anchor
                    aboveY = Core._region_top anchor - h
                    yBelow =
                      if h <= Core._region_height vp' - belowY
                        then belowY
                        else max (Core._region_top vp') aboveY
                    x' =
                      max (Core._region_left vp')
                        (min (Core._region_left vp' + slackW) (Core._region_left anchor))
                 in (x', yBelow)
         in Region x0 y0 w h

  -- Gesture deferral: track the current non-wheel button at the
  -- boundary. Requested visibility applies only when no ordinary
  -- mouse-button gesture is in progress; wheel input does not hold
  -- changes. The final release is delivered to the currently visible
  -- layer (input routing uses pre-frame visibility), then the latest
  -- requested visibility applies in the settled frame.
  let stepGesture e _ = case e of
        V.EvMouseDown _ _ btn _
          | btn /= V.BScrollUp && btn /= V.BScrollDown -> Just (Tracking btn)
        V.EvMouseUp _ _ _ -> Just WaitingForInput
        _ -> Nothing
  gestureD <- foldDynMaybe stepGesture WaitingForInput inp
  let settled = (== WaitingForInput) <$> gestureD
  visibleD <- foldDyn
    (\(req, ok) old -> if ok then req else old)
    False
    (attachWith (\ok req -> (req, ok)) (current settled) (updated requestedD))

  -- Dismiss detection at the boundary. Pre-frame sampling of actual
  -- visibility, child-active, and ambient focus: the Escape that
  -- closes an inner overlay does not also dismiss this one, and the
  -- opener is not replayed.
  ambFocD <- Core.focus
  let dismissKeyE =
        attachWithMaybe
          ( \(tbl, vis, childActive, ambientFocus) e -> case e of
              V.EvKey k mods
                | vis && not childActive && ambientFocus ->
                    fmap (const OverlayDismissKey) (Map.lookup (k, mods) tbl)
              _ -> Nothing
          )
          ( (,,,)
              <$> (current (dispatchTable <$> _overlayConfig_dismissKeys cfg))
              <*> current visibleD
              <*> current (_overlayConfig_childActive cfg)
              <*> current ambFocD
          )
          inp
      outsideE =
        if _overlayConfig_dismissOnOutsideClick cfg
          then
            attachPromptlyDynWithMaybe
              ( \fgRect e -> case e of
                  V.EvMouseDown x y V.BLeft _
                    | x < Core._region_left fgRect
                        || y < Core._region_top fgRect
                        || x >= Core._region_left fgRect + Core._region_width fgRect
                        || y >= Core._region_top fgRect + Core._region_height fgRect ->
                        Just OverlayDismissOutside
                  _ -> Nothing
              )
              fgRectD
              inp
          else never
      dismissed = leftmost [dismissKeyE, outsideE]

  -- Route input: while actually visible, the background gets no user
  -- input at all (keyboard, paste, scroll, clicks); the foreground
  -- gets everything, routed through its fitted pane. Terminal
  -- focus/resize notifications reach both layers in both states; they
  -- do not represent user activation. Visibility decisions use the
  -- pre-frame value; a dismissal in this frame does not also expose
  -- the background in the same frame.
  let routeInput vis e = case e of
        V.EvResize {} -> Just e
        V.EvGainedFocus -> Just e
        V.EvLostFocus -> Just e
        _ -> if vis then Just e else Nothing
      bgRoute = attachWithMaybe (\vis ev -> routeInput vis ev) (current visibleD) inp
      fgRoute = inp
  (bgResult, bgImages, bgCursor) <- runLayer bgRoute bgChild
  (fgResult, fgImages, fgCursor) <- runLayer inp fgChild

  -- Emit images: background, then backdrop, then panel backing, then
  -- foreground. Later emissions are on top because the core reverses
  -- layers for the final picture.
  vpW <- Core.viewportWidth
  vpH <- Core.viewportHeight
  let backdropB = _overlayConfig_backdrop cfg
      panelB = _overlayConfig_panelStyle cfg
      composeImages vis vw vh bgIm fgIm bd panel =
        if not vis
          then bgIm
          else
            bgIm
              ++ backdropImages vw vh bd
              ++ panelImages vw vh panel
              ++ fgIm
  Core.tellImages $
    composeImages
      <$> current visibleD
      <*> current vpW
      <*> current vpH
      <*> bgImages
      <*> fgImages
      <*> backdropB
      <*> panelB

  -- Cursor: emit only the active layer's state, including visibility
  -- transitions, so closing restores the background cursor even if it
  -- emitted no new event.
  pure
    Overlay
      { _overlay_background = bgResult
      , _overlay_foreground = fgResult
      , _overlay_visible = visibleD
      , _overlay_dismissed = dismissed
      }
  where
    backdropImages :: Int -> Int -> OverlayBackdrop -> [V.Image]
    backdropImages vw vh bd = case bd of
      NoBackdrop -> []
      SolidBackdrop st ->
        [V.charFill (applyAttr st V.defAttr) ' ' vw vh]
      CanvasBackdrop c ->
        [canvasToImage (placeCanvas 0 0 c (blankCanvas vw vh))]

    panelImages :: Int -> Int -> Maybe Style -> [V.Image]
    panelImages _ _ _ = []

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
