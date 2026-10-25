{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Vty.WidgetSpec (spec) where

import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Reflex.Vty.Host
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), imageText)
import Reflex.Vty.Widget

-- | Visible text of each grid row (rows are padded to the grid width).
gridText :: Grid -> [String]
gridText g = map (T.unpack . T.concat . map renderSpanText) (gridRows g)

spec :: Spec
spec = describe "Reflex.Vty.Widget" $ do
  describe "key, keys, keyCombo, keyCombos" $ do
    it "match exact keys and modifier sets and reject mismatches" $
      withApp defaultVtyAppConfig (20, 6) keyApp $ \driver -> do
        f1 <- stepInput driver (V.EvKey (V.KChar 'a') [])
        frameObservation f1 `shouldBe` "key:a"
        map (take 5) (take 1 (gridText (frameGrid f1))) `shouldBe` ["key:a"]

        -- 'key' is modifier-less: 'a' with Shift must not produce an event.
        f2 <- stepInput driver (V.EvKey (V.KChar 'a') [V.MShift])
        frameObservation f2 `shouldBe` "key:a"
        map (take 5) (take 1 (gridText (frameGrid f2))) `shouldBe` ["key:a"]

        -- 'keys' matches each listed modifier-less key.
        f3 <- stepInput driver (V.EvKey (V.KChar 'x') [])
        frameObservation f3 `shouldBe` "keys:(KChar 'x',[])"
        f4 <- stepInput driver (V.EvKey (V.KChar 'y') [])
        frameObservation f4 `shouldBe` "keys:(KChar 'y',[])"
        f5 <- stepInput driver (V.EvKey (V.KChar 'y') [V.MMeta])
        frameObservation f5 `shouldBe` "keys:(KChar 'y',[])"

        -- 'keyCombo' requires the exact modifier set.
        f6 <- stepInput driver (V.EvKey (V.KChar 'c') [V.MShift])
        frameObservation f6 `shouldBe` "combo:shift-c"
        f7 <- stepInput driver (V.EvKey (V.KChar 'c') [])
        frameObservation f7 `shouldBe` "combo:shift-c"
        f8 <- stepInput driver (V.EvKey (V.KChar 'c') [V.MCtrl])
        frameObservation f8 `shouldBe` "combo:shift-c"

        -- 'keyCombos' matches any listed combination.
        f9 <- stepInput driver (V.EvKey (V.KChar 'd') [V.MCtrl])
        frameObservation f9 `shouldBe` "combos:(KChar 'd',[MCtrl])"
        f10 <- stepInput driver (V.EvKey V.KEnter [])
        frameObservation f10 `shouldBe` "combos:(KEnter,[])"
        f11 <- stepInput driver (V.EvKey V.KEnter [V.MShift])
        frameObservation f11 `shouldBe` "combos:(KEnter,[])"
        f12 <- stepInput driver (V.EvKey (V.KChar 'd') [])
        frameObservation f12 `shouldBe` "combos:(KEnter,[])"
        stopApp driver

  describe "filterKeys" $ do
    it "drops keys the predicate rejects and passes everything else through unchanged" $
      withApp defaultVtyAppConfig (20, 6) filterApp $ \driver -> do
        f1 <- stepInput driver (V.EvKey (V.KChar 'q') [])
        frameObservation f1 `shouldBe` ("none" :: T.Text, 0 :: Int)
        f2 <- stepInput driver (V.EvKey (V.KChar 'q') [V.MCtrl])
        frameObservation f2 `shouldBe` ("none", 0)
        -- A non-rejected key passes (with its modifiers).
        f3 <- stepInput driver (V.EvKey (V.KChar 'z') [V.MShift])
        frameObservation f3 `shouldBe` ("EvKey (KChar 'z') [MShift]", 1)
        -- Non-key events pass through unfiltered and unchanged. The
        -- root consumes EvResize itself. Paste is the non-key event a
        -- widget actually observes.
        f4 <- stepInput driver (V.EvPaste "hi")
        frameObservation f4 `shouldBe` ("EvPaste \"hi\"", 2)
        stopApp driver

  describe "gainedFocus and lostFocus" $ do
    it "extract terminal focus events from the input stream" $
      withApp defaultVtyAppConfig (20, 6) focusApp $ \driver -> do
        f1 <- stepInput driver V.EvGainedFocus
        frameObservation f1 `shouldBe` ("gained" :: T.Text)
        map (take 6) (take 1 (gridText (frameGrid f1))) `shouldBe` ["gained"]
        f2 <- stepInput driver V.EvLostFocus
        frameObservation f2 `shouldBe` "lost"
        map (take 4) (take 1 (gridText (frameGrid f2))) `shouldBe` ["lost"]
        -- Keys are not focus events.
        f3 <- stepInput driver (V.EvKey (V.KChar 'k') [])
        frameObservation f3 `shouldBe` "lost"
        stopApp driver

  describe "pane input semantics" $ do
    it "delivers keys only to the focused pane and renders each pane in its region" $
      withApp defaultVtyAppConfig (20, 6) panesApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) -> do
            let firstRows = take 1 (gridText (frameGrid f0))
            map (take 1) firstRows `shouldBe` ["L"]
            map (take 1 . drop 10) firstRows `shouldBe` ["R"]
            frameObservation f0
              `shouldBe` (("none", Nothing), ("none", Nothing))
          [] -> expectationFailure "expected an initial frame"
        f1 <- stepInput driver (V.EvKey (V.KChar 'a') [])
        frameObservation f1 `shouldBe` (("(KChar 'a',[])", Nothing), ("none", Nothing))
        stopApp driver

    it "includes the last column of a pane and hands the next column to its neighbor" $
      withApp defaultVtyAppConfig (20, 6) panesApp $ \driver -> do
        -- The left pane's rightmost column (x = 9) is inside the left
        -- pane and outside the right one.
        f1 <- stepInput driver (V.EvMouseDown 9 2 V.BLeft [])
        frameObservation f1 `shouldBe` (("none", Just (9, 2)), ("none", Nothing))
        stopApp driver

    it "delivers a click on the neighbor pane's left edge to that neighbor" $
      withApp defaultVtyAppConfig (20, 6) panesApp $ \driver -> do
        -- One column further (x = 10): exactly the right pane's origin,
        -- reported pane-locally as (0, 2) and invisible to the left pane.
        f1 <- stepInput driver (V.EvMouseDown 10 2 V.BLeft [])
        frameObservation f1 `shouldBe` (("none", Nothing), ("none", Just (0, 2)))
        stopApp driver

    it "drops clicks one past the region edge" $
      withApp defaultVtyAppConfig (20, 6) panesApp $ \driver -> do
        -- One past the right pane's right edge (x = 20 of a 20-wide root).
        f1 <- stepInput driver (V.EvMouseDown 20 2 V.BLeft [])
        frameObservation f1 `shouldBe` (("none", Nothing), ("none", Nothing))
        stopApp driver

    it "reports drags that begin inside the pane and continue outside it" $
      withApp defaultVtyAppConfig (20, 6) panesApp $ \driver -> do
        f1 <- stepInput driver (V.EvMouseDown 5 1 V.BLeft [])
        frameObservation f1 `shouldBe` (("none", Just (5, 1)), ("none", Nothing))
        -- Drag continues outside the left pane, into the right pane's area:
        -- still delivered to the tracking (left) pane, never to the right.
        f2 <- stepInput driver (V.EvMouseDown 15 1 V.BLeft [])
        frameObservation f2 `shouldBe` (("none", Just (15, 1)), ("none", Nothing))
        stopApp driver

    it "ignores drags that begin outside the pane even when they enter it" $
      withApp defaultVtyAppConfig (20, 6) panesApp $ \driver -> do
        f1 <- stepInput driver (V.EvMouseDown 15 1 V.BLeft [])
        frameObservation f1 `shouldBe` (("none", Nothing), ("none", Just (5, 1)))
        -- Now the drag moves into the left pane. The left pane was not
        -- tracking, so it receives no input.
        f2 <- stepInput driver (V.EvMouseDown 5 1 V.BLeft [])
        frameObservation f2 `shouldBe` (("none", Nothing), ("none", Just (-5, 1)))
        stopApp driver

    it "drops buttonless mouse-up unless a drag is being tracked" $
      withApp defaultVtyAppConfig (20, 6) panesApp $ \driver -> do
        f1 <- stepInput driver (V.EvMouseUp 5 1 Nothing)
        frameObservation f1 `shouldBe` (("none", Nothing), ("none", Nothing))
        -- After a tracked button-down, a buttonless up is delivered.
        _ <- stepInput driver (V.EvMouseDown 5 1 V.BLeft [])
        f3 <- stepInput driver (V.EvMouseUp 6 2 Nothing)
        frameObservation f3 `shouldBe` (("none", Just (6, 2)), ("none", Nothing))
        stopApp driver

    it "keeps delivering keys through zero- and negative-width panes but renders nothing" $
      withApp defaultVtyAppConfig (20, 6) zeroPaneApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) -> do
            mapM_ (\row -> row `shouldBe` replicate 20 ' ') (gridText (frameGrid f0))
            frameObservation f0
              `shouldBe` (("none", Nothing), ("none", Nothing))
          [] -> expectationFailure "expected an initial frame"
        -- Key delivery depends on focus, not on region size.
        f1 <- stepInput driver (V.EvKey (V.KChar 'k') [])
        frameObservation f1
          `shouldBe` (("(KChar 'k',[])", Nothing), ("(KChar 'k',[])", Nothing))
        -- A click exactly on the zero-width pane's left edge is outside it.
        f2 <- stepInput driver (V.EvMouseDown 3 1 V.BLeft [])
        frameObservation f2
          `shouldBe` (("(KChar 'k',[])", Nothing), ("(KChar 'k',[])", Nothing))
        stopApp driver

  describe "pane coordinate translation" $ do
    it "agrees between translated mouse input and absolute cursor/image output" $
      withApp defaultVtyAppConfig (20, 6) cursorPaneApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) -> do
            -- The child image and cursor appear at the pane origin (5, 2).
            frameCursor f0 `shouldBe` V.AbsoluteCursor 5 2
            gridText (frameGrid f0) !! 2 !! 5 `shouldBe` 'X'
          [] -> expectationFailure "expected an initial frame"
        -- Clicking exactly on the rendered 'X' reports the pane-local
        -- origin, matching where the image and cursor were drawn.
        f1 <- stepInput driver (V.EvMouseDown 5 2 V.BLeft [])
        frameObservation f1 `shouldBe` (Just (0, 0) :: Maybe (Int, Int))
        f2 <- stepInput driver (V.EvMouseDown 7 3 V.BLeft [])
        frameObservation f2 `shouldBe` Just (2, 1)
        stopApp driver

    it "drops clicks entirely outside the pane when nothing is being tracked" $
      withApp defaultVtyAppConfig (20, 6) cursorPaneApp $ \driver -> do
        -- One column left of the pane, before any drag begins.
        f1 <- stepInput driver (V.EvMouseDown 4 2 V.BLeft [])
        frameObservation f1 `shouldBe` (Nothing :: Maybe (Int, Int))
        stopApp driver

  describe "withinImage" $ do
    it "crops to the region size" $
      imageText (withinImage (Region 0 0 3 2) (V.string V.defAttr "hello"))
        `shouldBe` ["hel"]

    it "clamps zero-size regions to an empty image" $
      V.imageWidth (withinImage (Region 0 0 0 5) (V.string V.defAttr "hello"))
        `shouldBe` 0

    it "treats negative sizes as zero" $
      V.imageWidth (withinImage (Region 0 0 (-1) 5) (V.string V.defAttr "hello"))
        `shouldBe` 0

    it "clips rendered output through the host, including zero and negative widths" $
      withApp defaultVtyAppConfig (20, 6) regionApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) -> take 1 (gridText (frameGrid f0)) `shouldBe` ["abcdefghij          "]
          [] -> expectationFailure "expected an initial frame"
        f1 <- stepCommand driver 4
        take 1 (gridText (frameGrid f1)) `shouldBe` ["abcd                "]
        f2 <- stepCommand driver 0
        take 1 (gridText (frameGrid f2)) `shouldBe` [replicate 20 ' ']
        -- Negative widths are clamped, not crashes.
        f3 <- stepCommand driver (-3)
        take 1 (gridText (frameGrid f3)) `shouldBe` [replicate 20 ' ']
        frameObservation f3 `shouldBe` ("-3" :: T.Text)
        stopApp driver

  describe "imagesInRegion and mapImages" $ do
    it "crop and translate child images into the given region" $
      withApp defaultVtyAppConfig (20, 6) clipApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) -> do
            let rows = gridText (frameGrid f0)
            rows !! 0 `shouldBe` replicate 20 ' '
            take 8 (rows !! 1) `shouldBe` "  abcdef"
          [] -> expectationFailure "expected an initial frame"
        f1 <- stepCommand driver 3
        take 5 (gridText (frameGrid f1) !! 1) `shouldBe` "  abc"
        f2 <- stepCommand driver 0
        gridText (frameGrid f2) !! 1 `shouldBe` replicate 20 ' '
        stopApp driver

  describe "localRegion and localLayoutRegion" $ do
    it "splits layout and viewport regions as requested" $
      withApp defaultVtyAppConfig (20, 6) layoutApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) ->
            frameObservation f0
              `shouldBe` ((Region 0 0 20 100, Region 0 0 20 6), Region 1 1 18 4)
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

  describe "captureImages" $ do
    it "hides child images until re-emitted and keeps cursor and screen-mode requests" $
      withApp defaultVtyAppConfig (20, 6) captureApp $ \driver -> do
        fs0 <- driverFrames driver
        case reverse fs0 of
          (f0 : _) -> do
            frameObservation f0 `shouldBe` ("hidden" :: T.Text)
            frameCursor f0 `shouldBe` V.AbsoluteCursor 0 0
          [] -> expectationFailure "expected an initial frame"
        -- The alternate-screen request made inside the captured child
        -- must still reach the terminal.
        log0 <- driverOutputLog driver
        log0 `shouldSatisfy` any (isInfixOf "?1049h")
        -- Turning the marker on changes only the captured behavior: the
        -- marker must stay invisible, but the cursor must persist.
        f1 <- stepCommand driver 1
        frameObservation f1 `shouldBe` "shown"
        frameCursor f1 `shouldBe` V.AbsoluteCursor 0 0
        gridText (frameGrid f1) !! 0 `shouldBe` replicate 20 ' '
        -- Re-emitting draws exactly one correctly placed marker.
        f2 <- stepCommand driver 2
        frameObservation f2 `shouldBe` "reemitted"
        let rows = gridText (frameGrid f2)
        take 1 rows `shouldBe` ["MARK                "]
        concat rows `shouldSatisfy` (\s -> length (filter (== 'M') s) == 1)
        stopApp driver

------------------------------------------------------------------------------
-- Applications under test
------------------------------------------------------------------------------

-- | Records which key matcher fired last and renders the label.
keyApp :: TestApp () T.Text
keyApp _cmd = do
  aK <- key (V.KChar 'a')
  xyK <- keys [V.KChar 'x', V.KChar 'y']
  cCombo <- keyCombo (V.KChar 'c', [V.MShift])
  dCombos <-
    keyCombos (Set.fromList [(V.KChar 'd', [V.MCtrl]), (V.KEnter, [])])
  let hit =
        leftmost
          [ "key:a" <$ aK
          , (\k -> "keys:" <> T.pack (show k)) <$> xyK
          , "combo:shift-c" <$ cCombo
          , (\kc -> "combos:" <> T.pack (show kc)) <$> dCombos
          ]
  lastHit <- holdDyn "none" hit
  tellImages (fmap (\s -> [V.string V.defAttr (T.unpack s)]) (current lastHit))
  pure (lastHit, never)

-- | A child under 'filterKeys' that records every event it still receives.
filterApp :: TestApp () (T.Text, Int)
filterApp _cmd = do
  seen <- filterKeys (\(k, _ms) -> k /= V.KChar 'q') $ do
    inp <- input
    lastE <- holdDyn "none" (T.pack . show <$> inp)
    nSeen <- foldDyn (\_ c -> c + 1) (0 :: Int) inp
    pure ((,) <$> lastE <*> nSeen)
  pure (seen, never)

-- | Records terminal focus changes and renders them.
focusApp :: TestApp () T.Text
focusApp _cmd = do
  g <- gainedFocus
  l <- lostFocus
  d <- holdDyn "none" (leftmost ["gained" <$ g, "lost" <$ l])
  tellImages (fmap (\s -> [V.string V.defAttr (T.unpack s)]) (current d))
  pure (d, never)

-- | One pane child: last key, last mouse position (pane-local), and a
-- one-character marker at the pane origin.
paneChild
  :: (Reflex t, MonadHold t m, HasInput t m, HasImageWriter t m)
  => Char
  -> m (Dynamic t (T.Text, Maybe (Int, Int)))
paneChild c = do
  inp <- input
  k <- holdDyn "none" $ fforMaybe inp $ \case
    V.EvKey key' mods -> Just (T.pack (show (key', mods)))
    _ -> Nothing
  m <- holdDyn Nothing $ fforMaybe inp $ \case
    V.EvMouseDown x y _ _ -> Just (Just (x, y))
    V.EvMouseUp x y _ -> Just (Just (x, y))
    _ -> Nothing
  tellImages (pure [V.string V.defAttr [c]])
  pure ((,) <$> k <*> m)

-- | A focused left pane and an unfocused right pane, side by side.
panesApp :: TestApp () ((T.Text, Maybe (Int, Int)), (T.Text, Maybe (Int, Int)))
panesApp _cmd = do
  l <- pane (pure (Region 0 0 10 6)) (pure True) (paneChild 'L')
  r <- pane (pure (Region 10 0 10 6)) (pure False) (paneChild 'R')
  pure ((,) <$> l <*> r, never)

-- | Two focused but degenerate (zero- and negative-width) panes.
zeroPaneApp :: TestApp () ((T.Text, Maybe (Int, Int)), (T.Text, Maybe (Int, Int)))
zeroPaneApp _cmd = do
  z <- pane (pure (Region 3 1 0 2)) (pure True) (paneChild 'Z')
  n <- pane (pure (Region 3 1 (-2) 2)) (pure True) (paneChild 'N')
  pure ((,) <$> z <*> n, never)

-- | A pane whose child draws an 'X' at the pane origin and requests a
-- visible cursor there, reporting pane-local click coordinates.
cursorPaneApp :: TestApp () (Maybe (Int, Int))
cursorPaneApp _cmd = do
  m <- pane (pure (Region 5 2 10 3)) (pure True) $ do
    inp <- input
    clicks <- holdDyn Nothing $ fforMaybe inp $ \case
      V.EvMouseDown x y _ _ -> Just (Just (x, y))
      _ -> Nothing
    tellImages (pure [V.string V.defAttr "X"])
    setCursor (pure (CursorState CursorVisible CursorStyleDefault (0, 0)))
    pure clicks
  pure (m, never)

-- | Renders text clipped by 'withinImage' to a command-chosen width.
regionApp :: TestApp Int T.Text
regionApp cmd = do
  w <- holdDyn 20 cmd
  tellImages
    (fmap (\wi -> [withinImage (Region 0 0 wi 1) (V.string V.defAttr "abcdefghij")]) (current w))
  pure (T.pack . show <$> w, never)

-- | Child images cropped and translated by 'imagesInRegion' via
-- 'mapImages', with the region width driven by commands.
clipApp :: TestApp Int T.Text
clipApp cmd = do
  w <- holdDyn 6 cmd
  let reg = (\wi -> Region 2 1 wi 2) <$> w
  mapImages (imagesInRegion (current reg)) $
    tellImages (pure [V.string V.defAttr "abcdefghij"])
  pure (T.pack . show <$> w, never)

-- | Exercises 'localLayoutRegion' (layout inflated, viewport untouched)
-- and 'localRegion' (both shrink) and reports what the child observed.
layoutApp :: TestApp () ((Region, Region), Region)
layoutApp _cmd = do
  pair <- localLayoutRegion (fmap (\r -> r {_region_height = 100})) $ do
    a <- askRegion
    b <- askViewport
    pure ((,) <$> a <*> b)
  shrunk <- localRegion (fmap shrinkBy2) askRegion
  pure ((,) <$> pair <*> shrunk, never)
  where
    shrinkBy2 (Region l t w h) = Region (l + 1) (t + 1) (max 0 (w - 2)) (max 0 (h - 2))

-- | The child's images are captured: a marker drawn by the child only
-- appears once the parent re-emits the captured behavior. The child's
-- cursor and alternate-screen requests bypass the capture.
captureApp :: TestApp Int T.Text
captureApp cmd = do
  ((), captured) <- captureImages $ do
    markOn <- holdDyn False ((== (1 :: Int)) <$> cmd)
    tellImages (fmap (\b -> [V.string V.defAttr "MARK" | b]) (current markOn))
    setCursor (pure (CursorState CursorVisible CursorStyleDefault (0, 0)))
    enterAlternateScreen
  let again = ffilter (== (2 :: Int)) cmd
  reemit <- holdDyn [] (tag captured again)
  tellImages (current reemit)
  obs <-
    holdDyn
      "hidden"
      (leftmost ["shown" <$ ffilter (== (1 :: Int)) cmd, "reemitted" <$ again])
  pure (obs, never)
