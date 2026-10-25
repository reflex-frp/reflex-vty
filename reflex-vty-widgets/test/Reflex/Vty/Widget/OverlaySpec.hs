{-# LANGUAGE PackageImports #-}
module Reflex.Vty.Widget.OverlaySpec (spec) where

import Control.Monad (void)
import Data.Default (def)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Reflex.Vty.Host (defaultVtyAppConfig, CursorVisibility (..), CursorStyle (..))
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (gridRows, renderSpanText, attrAt)
import "reflex-vty" Reflex.Vty.Widget
import Reflex.Vty.Style (withBackground)
import Reflex.Vty.Canvas (blankCanvas, imageToCanvas, placeCanvas)
import Reflex.Vty.Widget.Overlay

-- Real production-stack layers record the input that crosses each boundary.
data Command = ShowLayer | HideLayer | BackgroundCursor | ForegroundCursor
  deriving (Eq, Show)
type Observation = (Bool, [V.Event], [V.Event], Region)

lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame d = last <$> driverFrames d

row :: Int -> Frame o -> T.Text
row y = T.concat . map renderSpanText . (!! y) . gridRows . frameGrid

visible :: Frame Observation -> Bool
visible f = let (v, _, _, _) = frameObservation f in v
logs :: Frame Observation -> ([V.Event], [V.Event])
logs f = let (_, b, g, _) = frameObservation f in (b, g)

spec :: Spec
spec = describe "Reflex.Vty.Widget.Overlay" $ do
  it "mounts an initially visible titled dialog at one translated origin" $
    withApp defaultVtyAppConfig (20,12) (overlayApp True True False) $ \d -> do
      f <- lastFrame d
      visible f `shouldBe` True
      T.take 8 (T.drop 6 (row 3 f)) `shouldBe` "┌─Test─┐" -- title fills the six-cell top edge
      T.take 4 (T.drop 7 (row 4 f)) `shouldBe` "BODY"
      let (_,_,_,r) = frameObservation f
      r `shouldBe` Region 7 4 6 4
      frameCursor f `shouldBe` V.AbsoluteCursor 8 5
      void $ stepInput d (V.EvMouseDown 8 5 V.BLeft [])
      clicked <- stepInput d (V.EvMouseUp 8 5 (Just V.BLeft))
      logs clicked `shouldBe`
        ([], [V.EvMouseDown 1 1 V.BLeft [], V.EvMouseUp 1 1 (Just V.BLeft)])
      stopApp d

  it "isolates hidden and visible input, consumes dismissals, and restores cached cursors" $
    withApp defaultVtyAppConfig (20,12) (overlayApp False False False) $ \d -> do
      f0 <- lastFrame d
      frameCursor f0 `shouldBe` V.AbsoluteCursor 3 2
      f1 <- stepInput d (V.EvKey (V.KChar 'o') [])
      visible f1 `shouldBe` True
      logs f1 `shouldBe` ([V.EvKey (V.KChar 'o') []], [])
      f2 <- stepInput d (V.EvPaste "saved")
      logs f2 `shouldBe` ([V.EvKey (V.KChar 'o') []], [V.EvPaste "saved"])
      f3 <- stepCommand d BackgroundCursor
      frameCursor f3 `shouldBe` V.AbsoluteCursor 7 4
      f4 <- stepInput d (V.EvKey V.KEsc [])
      visible f4 `shouldBe` False
      logs f4 `shouldBe` logs f2
      frameCursor f4 `shouldBe` V.AbsoluteCursor 5 2
      f5 <- stepCommand d ForegroundCursor
      frameCursor f5 `shouldBe` V.AbsoluteCursor 5 2
      f6 <- stepInput d (V.EvPaste "background")
      logs f6 `shouldBe` ([V.EvKey (V.KChar 'o') [],V.EvPaste "background"], [V.EvPaste "saved"])
      stopApp d

  it "settles only the captured button and applies the newest pending request" $
    withApp defaultVtyAppConfig (20,12) (overlayApp False False True) $ \d -> do
      f1 <- stepInput d (V.EvMouseDown 4 3 V.BLeft [])
      visible f1 `shouldBe` False
      void $ stepInput d (V.EvMouseDown 4 3 V.BRight [])
      f2 <- stepInput d (V.EvMouseUp 4 3 (Just V.BRight))
      visible f2 `shouldBe` False
      f3 <- stepInput d (V.EvMouseDown 4 3 V.BScrollDown [])
      visible f3 `shouldBe` False
      f4 <- stepCommand d HideLayer
      visible f4 `shouldBe` False
      f5 <- stepCommand d ShowLayer
      visible f5 `shouldBe` False
      f6 <- stepInput d (V.EvMouseUp 4 3 Nothing)
      visible f6 `shouldBe` True
      fst (logs f6) `shouldSatisfy` elem (V.EvMouseUp 1 1 Nothing)
      snd (logs f6) `shouldBe` []
      void $ stepCommand d HideLayer
      void $ stepInput d (V.EvMouseDown 4 3 V.BScrollUp [])
      f7 <- stepCommand d ShowLayer
      visible f7 `shouldBe` True
      stopApp d

  it "dismisses only a visible outside-left boundary and does not replay its release" $
    withApp defaultVtyAppConfig (20,12) (overlayApp False False False) $ \d -> do
      void $ stepInput d (V.EvMouseDown 4 3 V.BLeft [])
      f0 <- stepInput d (V.EvMouseUp 4 3 Nothing)
      visible f0 `shouldBe` False
      void $ stepCommand d ShowLayer
      f1 <- stepInput d (V.EvMouseDown 4 3 V.BLeft [])
      visible f1 `shouldBe` True
      f2 <- stepInput d (V.EvMouseUp 4 3 Nothing)
      visible f2 `shouldBe` False
      logs f2 `shouldBe` logs f0
      stopApp d

  it "keeps tiny bodies inert, forwards notifications, and retains boundary Escape" $
    withApp defaultVtyAppConfig (20,12) tinyApp $ \d -> do
      f0 <- lastFrame d
      visible f0 `shouldBe` True
      void $ resizeApp d (2,2)
      f1 <- stepInput d (V.EvKey (V.KChar 'x') [])
      let (_,_,fg,r) = frameObservation f1
      r `shouldBe` Region 1 1 0 0
      fg `shouldSatisfy` notElem (V.EvKey (V.KChar 'x') [])
      frameCursor f1 `shouldBe` V.NoCursor
      void $ stepInput d (V.EvPaste "blocked")
      void $ stepInput d (V.EvMouseDown 0 0 V.BLeft [])
      inert <- stepInput d (V.EvMouseUp 0 0 Nothing)
      snd (logs inert) `shouldBe` fg
      void $ resizeApp d (1,1)
      f2 <- stepInput d V.EvLostFocus
      let (b,g) = logs f2
      b `shouldSatisfy` elem V.EvLostFocus
      g `shouldSatisfy` elem V.EvLostFocus
      void $ stepInput d V.EvGainedFocus
      f3 <- stepInput d (V.EvKey V.KEsc [])
      visible f3 `shouldBe` False
      void $ resizeApp d (20,12)
      f4 <- stepCommand d ShowLayer
      T.take 4 (T.drop 7 (row 4 f4)) `shouldBe` "BODY"
      stopApp d

  it "composes backdrop behind a panel confined to the fitted rectangle" $
    withApp defaultVtyAppConfig (20,12) (overlayApp True False False) $ \d -> do
      f <- lastFrame d
      (V.fixedBackColor <$> attrAt 3 2 (frameGrid f)) `shouldBe` Just (Just V.blue)
      (V.fixedBackColor <$> attrAt 13 8 (frameGrid f)) `shouldBe` Just (Just V.red)
      (V.fixedBackColor <$> attrAt 14 8 (frameGrid f)) `shouldBe` Just (Just V.blue)
      stopApp d

  it "clamps local positions and flips an anchored panel above its anchor" $ do
    let check :: OverlayPosition -> Region -> IO ()
        check position expected =
          withApp defaultVtyAppConfig (20,12) (geometryApp position False) $ \d -> do
            f <- lastFrame d
            frameObservation f `shouldBe` expected
            stopApp d
    check (OverlayAt 99 (-5)) (Region 9 2 8 6)
    check (OverlayBelow (Region 12 6 2 1)) (Region 9 2 8 6)
    check (OverlayBelow (Region 1 0 2 1)) (Region 4 3 8 6)

  it "preserves transparent canvas gaps and a transparent panel" $
    withApp defaultVtyAppConfig (20,12) (geometryApp OverlayCentered True) $ \d -> do
      f <- lastFrame d
      T.take 4 (T.drop 3 (row 2 f)) `shouldBe` "BXBB"
      T.take 4 (T.drop 6 (row 4 f)) `shouldBe` "BBBB"
      (V.fixedBackColor <$> attrAt 4 2 (frameGrid f)) `shouldBe` Just (Just V.green)
      stopApp d

  it "gives a nested child sole ownership of Escape and outside clicks" $
    withApp defaultVtyAppConfig (20,12) nestedApp $ \d -> do
      f0 <- lastFrame d
      frameObservation f0 `shouldBe` (True,False,[])
      void $ stepInput d (V.EvKey (V.KChar 'p') [])
      f1 <- stepInput d (V.EvPaste "blocked")
      frameObservation f1 `shouldBe` (True,True,[V.EvKey (V.KChar 'p') []])
      f2 <- stepInput d (V.EvKey V.KEsc [])
      frameObservation f2 `shouldBe` (True,False,[V.EvKey (V.KChar 'p') []])
      void $ stepInput d (V.EvKey (V.KChar 'p') [])
      -- An outside ancestor click cannot dismiss it while its child is active.
      void $ stepInput d (V.EvMouseDown 0 0 V.BLeft [])
      outside <- stepInput d (V.EvMouseUp 0 0 Nothing)
      frameObservation outside `shouldBe` (True,True,replicate 2 (V.EvKey (V.KChar 'p') []))
      -- Inside the dialog body, outside the popup: dismiss only the popup.
      void $ stepInput d (V.EvMouseDown 15 8 V.BLeft [])
      f3 <- stepInput d (V.EvMouseUp 15 8 Nothing)
      frameObservation f3 `shouldBe` (True,False,replicate 2 (V.EvKey (V.KChar 'p') []))
      f4 <- stepInput d (V.EvKey V.KEsc [])
      frameObservation f4 `shouldBe` (False,False,replicate 2 (V.EvKey (V.KChar 'p') []))
      stopApp d

overlayApp :: Bool -> Bool -> Bool -> TestApp Command Observation
overlayApp initial titled downOpens commands =
  pane (pure (Region 3 2 14 8)) (pure True) $ boundary initial titled downOpens commands

tinyApp :: TestApp Command Observation
tinyApp = boundary True True False

boundary :: TestWidget t m => Bool -> Bool -> Bool -> Event t Command -> m (Dynamic t Observation, Event t ())
boundary initial titled downOpens commands = do
  rec requested <- holdDyn initial $ leftmost
        [False <$ _overlay_dismissed overlay, True <$ openE,
         True <$ ffilter (== ShowLayer) commands, False <$ ffilter (== HideLayer) commands]
      overlay <- (if titled then \c r -> dialog c (pure "Test") r else modal)
        (def { _overlayConfig_size = pure (8,6)
             , _overlayConfig_dismissOnOutsideClick = True
             , _overlayConfig_backdrop = pure (SolidBackdrop (withBackground V.blue def))
             , _overlayConfig_panelStyle = pure (Just (withBackground V.red def)) })
        requested
        (do
          e <- input
          events <- foldDyn (\v vs -> vs ++ [v]) [] e
          cursor <- holdDyn (0,0) ((2,0) <$ ffilter (== BackgroundCursor) commands)
          setCursor (CursorState CursorVisible CursorStyleDefault <$> cursor)
          tellImages (pure [V.string V.defAttr "BACKGROUND"])
          let open = ffilter (\v -> v == V.EvKey (V.KChar 'o') [] ||
                       (downOpens && case v of V.EvMouseDown _ _ V.BLeft _ -> True; _ -> False)) e
          pure (events, open))
        (do
          e <- input
          events <- foldDyn (\v vs -> vs ++ [v]) [] e
          r <- askViewport
          cursor <- holdDyn (1,1) ((2,1) <$ ffilter (== ForegroundCursor) commands)
          setCursor (CursorState CursorVisible CursorStyleDefault <$> cursor)
          tellImages (pure [V.string V.defAttr "BODY"])
          pure (events,r))
      let (bgEvents,openE) = _overlay_background overlay
          (fgEvents,bodyRegion) = _overlay_foreground overlay
  pure ((,,,) <$> _overlay_visible overlay <*> bgEvents <*> fgEvents <*> bodyRegion, never)

geometryApp :: OverlayPosition -> Bool -> TestApp () Region
geometryApp position transparent _ =
  pane (pure (Region 3 2 14 8)) (pure True) $ do
    overlay <- modal
      (def { _overlayConfig_size = pure (8,6)
           , _overlayConfig_position = pure position
           , _overlayConfig_panelStyle = pure (if transparent then Nothing else Just def)
           , _overlayConfig_backdrop = pure $ CanvasBackdrop $
               placeCanvas 1 0 (imageToCanvas (V.char (V.withBackColor V.defAttr V.green) 'X')) (blankCanvas 14 8) })
      (pure True)
      (tellImages (pure [V.charFill V.defAttr 'B' (14 :: Int) 8]))
      askViewport
    pure (_overlay_foreground overlay, never)

nestedApp :: TestApp () (Bool, Bool, [V.Event])
nestedApp _ = do
  rec outerRequest <- holdDyn True (False <$ _overlay_dismissed outer)
      outer <- dialog
        (def { _overlayConfig_size = pure (14,10)
             , _overlayConfig_dismissOnOutsideClick = True
             , _overlayConfig_childActive = innerVisible })
        (pure "Outer") outerRequest blank $ do
          rec innerRequest <- holdDyn False $ leftmost
                [False <$ _overlay_dismissed inner, True <$ openE]
              inner <- popover (defaultPopoverConfig { _overlayConfig_size = pure (6,3) })
                (pure (Region 0 1 6 1)) innerRequest
                (do
                  e <- input
                  received <- foldDyn (\v vs -> vs ++ [v]) [] e
                  pure (received, ffilter (== V.EvKey (V.KChar 'p') []) e))
                blank
              let (formEvents,openE) = _overlay_background inner
          pure (_overlay_visible inner, formEvents)
      let (innerVisible,events) = _overlay_foreground outer
  pure ((,,) <$> _overlay_visible outer <*> innerVisible <*> events, never)
