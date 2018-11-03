{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Vty.Widget
  ( VtyWidgetCtx(..)
  , VtyWidget(..)
  , VtyWidgetOut(..)
  , runVtyWidget
  , mainWidget
  , mainWidgetWithHandle
  , HasDisplaySize(..)
  , displayWidth
  , displayHeight
  , HasFocus(..)
  , HasVtyInput(..)
  , Region(..)
  , regionSize
  , regionBlankImage
  , Drag (..)
  , drag
  , mouseDown
  , pane
  , modifyImages
  , tellImages
  , tellShutdown
  , wrapText
  , splitV
  , splitVDrag
  , fractionSz
  , box
  , text
  , display
  , hyphenBoxStyle
  , singleBoxStyle
  , roundedBoxStyle
  , thickBoxStyle
  , doubleBoxStyle
  , fill
  , hRule
  , TextInputConfig(..)
  , textInput
  , multilineTextInput
  , def
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks, ask)
import Control.Monad.Trans.Writer (WriterT, runWriterT, censor, tell)
import Data.Char
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty (Image, Attr)
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Host

-- | The context within which a 'VtyWidget' runs
data VtyWidgetCtx t = VtyWidgetCtx
  { _vtyWidgetCtx_size :: Dynamic t (Int,Int)
    -- ^ The width and height of the region allocated to the widget.
  , _vtyWidgetCtx_focus :: Dynamic t Bool
    -- ^ Whether the widget should behave as if it has focus for keyboard input.
  , _vtyWidgetCtx_input :: Event t VtyEvent
    -- ^ User input events that the widget's parent chooses to share. These will generally
    -- be filtered for relevance:
    --  * Keyboard inputs are restricted to focused widgets
    --  * Mouse inputs are restricted to the region in which the widget resides and are
    --  translated into its internal coordinates.
  }

-- | The output of a 'VtyWidget'
data VtyWidgetOut t = VtyWidgetOut
  { _vtyWidgetOut_images :: Behavior t [Image]
    -- ^ The visual output of the 'VtyWidget'
  , _vtyWidgetOut_shutdown :: Event t ()
  }

instance Reflex t => Semigroup (VtyWidgetOut t) where
  wo <> wo' = VtyWidgetOut
    { _vtyWidgetOut_images = _vtyWidgetOut_images wo <> _vtyWidgetOut_images wo'
    , _vtyWidgetOut_shutdown = _vtyWidgetOut_shutdown wo <> _vtyWidgetOut_shutdown wo'
    }

instance (Reflex t) => Monoid (VtyWidgetOut t) where
  mempty = VtyWidgetOut mempty mempty
  mappend wo wo' = wo <> wo'

newtype VtyWidget t m a = VtyWidget { unVtyWidget :: WriterT (VtyWidgetOut t) (ReaderT (VtyWidgetCtx t) m) a }
  deriving (Functor, Applicative, Monad, MonadSample t, MonadHold t, MonadFix)

-- | Runs a 'VtyWidget' with a given context
runVtyWidget :: (Reflex t)
  => VtyWidgetCtx t
  -> VtyWidget t m a
  -> m (a, VtyWidgetOut t)
runVtyWidget ctx w = runReaderT (runWriterT (unVtyWidget w)) ctx

-- | Sets up the top-level context for a 'VtyWidget' and runs it with that context
mainWidgetWithHandle :: V.Vty -> (forall t m. MonadVtyApp t m => VtyWidget t m ()) -> IO ()
mainWidgetWithHandle vty child =
  runVtyAppWithHandle vty $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let inp' = fforMaybe inp $ \case
          V.EvResize {} -> Nothing
          x -> Just x
    let ctx = VtyWidgetCtx
          { _vtyWidgetCtx_size = size
          , _vtyWidgetCtx_input = inp'
          , _vtyWidgetCtx_focus = constDyn True
          }
    ((), wo) <- runVtyWidget ctx child
    return $ VtyResult
      { _vtyResult_picture = fmap (V.picForLayers . reverse) (_vtyWidgetOut_images wo)
      , _vtyResult_shutdown = _vtyWidgetOut_shutdown wo
      }

-- | Like 'mainWidgetWithHandle', but uses a default vty configuration
mainWidget :: (forall t m. MonadVtyApp t m => VtyWidget t m ()) -> IO ()
mainWidget child = do
  vty <- getDefaultVty
  mainWidgetWithHandle vty child

class (Reflex t, Monad m) => HasDisplaySize t m | m -> t where
  displaySize :: m (Dynamic t (Int, Int))

instance (Reflex t, Monad m) => HasDisplaySize t (VtyWidget t m) where
  displaySize = VtyWidget . lift $ asks _vtyWidgetCtx_size

displayWidth :: HasDisplaySize t m => m (Dynamic t Int)
displayWidth = fmap fst <$> displaySize

displayHeight :: HasDisplaySize t m => m (Dynamic t Int)
displayHeight = fmap snd <$> displaySize

class HasVtyInput t m | m -> t where
  input :: m (Event t VtyEvent)

instance (Reflex t, Monad m) => HasVtyInput t (VtyWidget t m) where
  input = VtyWidget . lift $ asks _vtyWidgetCtx_input

class HasFocus t m | m -> t where
  focus :: m (Dynamic t Bool)

instance (Reflex t, Monad m) => HasFocus t (VtyWidget t m) where
  focus = VtyWidget . lift $ asks _vtyWidgetCtx_focus

class (Reflex t, Monad m) => ImageWriter t m | m -> t where
  tellImages :: Behavior t [Image] -> m ()

instance (Reflex t, Monad m) => ImageWriter t (VtyWidget t m) where
  tellImages imgs = VtyWidget $ tell (mempty { _vtyWidgetOut_images = imgs })

class (Reflex t, Monad m) => Shutdown t m where
  tellShutdown :: Event t () -> m ()

instance (Reflex t, Monad m) => Shutdown t (VtyWidget t m) where
  tellShutdown sd = VtyWidget $ tell (mempty { _vtyWidgetOut_shutdown = sd })

data Region = Region
  { _region_left :: Int
  , _region_top :: Int
  , _region_width :: Int
  , _region_height :: Int
  }
  deriving (Show, Read, Eq, Ord)

regionSize :: Region -> (Int, Int)
regionSize (Region _ _ w h) = (w, h)

pane
  :: (Reflex t, Monad m)
  => Dynamic t Region -- ^ Region into which we should draw the widget (in coordinates relative to our own)
  -> Dynamic t Bool -- ^ Whether the widget should be focused when the parent is.
  -> VtyWidget t m a
  -> VtyWidget t m a
pane reg foc child = VtyWidget $ do
  ctx <- lift ask
  let ctx' = VtyWidgetCtx
        { _vtyWidgetCtx_input = leftmost -- TODO: think about this leftmost more.
            [ fmapMaybe id $
                attachWith (\(r,f) e -> filterInput r f e)
                  (liftA2 (,) (current reg) (current foc))
                  (_vtyWidgetCtx_input ctx)
            ]
        , _vtyWidgetCtx_focus = liftA2 (&&) (_vtyWidgetCtx_focus ctx) foc
        , _vtyWidgetCtx_size = fmap regionSize reg }
  (result, wo) <- lift . lift $ runVtyWidget ctx' child
  let images = _vtyWidgetOut_images wo
      images' = liftA2 (\r is -> map (withinImage r) is) (current reg) images
      wo' = wo { _vtyWidgetOut_images = images' }
  tell wo'
  return result
  where
    -- Filters input such that:
    -- * unfocused widgets receive no key events
    -- * mouse inputs outside the region are ignored
    -- * mouse inputs inside the region have their coordinates translated
    -- such that (0,0) is the top-left corner of the region
    filterInput :: Region -> Bool -> VtyEvent -> Maybe VtyEvent
    filterInput (Region l t w h) focused e = case e of
      V.EvKey _ _ | not focused -> Nothing
      V.EvMouseDown x y btn m -> mouse (\u v -> V.EvMouseDown u v btn m) x y
      V.EvMouseUp x y btn -> mouse (\u v -> V.EvMouseUp u v btn) x y
      _ -> Just e
      where
        mouse con x y
          | or [ x < l
               , y < t
               , x >= l + w
               , y >= t + h ] = Nothing
          | otherwise =
            Just (con (x - l) (y - t))

data Drag = Drag
  { _drag_from :: (Int, Int) -- ^ Where the drag began
  , _drag_to :: (Int, Int) -- ^ Where the mouse currently is
  , _drag_button :: V.Button -- ^ Which mouse button is dragging
  , _drag_modifiers :: [V.Modifier] -- ^ What modifiers are held
  , _drag_end :: Bool -- ^ Whether the drag ended (the mouse button was released)
  }
  deriving (Eq, Ord, Show)

drag
  :: (Reflex t, MonadFix m, MonadHold t m)
  => V.Button
  -> VtyWidget t m (Event t Drag)
drag btn = do
  inp <- input
  let f :: Drag -> V.Event -> Maybe Drag
      f (Drag from _ _ mods end) = \case
        V.EvMouseDown x y btn' mods'
          | end         -> Just $ Drag (x,y) (x,y) btn' mods' False
          | btn == btn' -> Just $ Drag from (x,y) btn mods' False
          | otherwise   -> Nothing -- Ignore other buttons.
        V.EvMouseUp x y (Just btn')
          | end         -> Nothing
          | btn == btn' -> Just $ Drag from (x,y) btn mods True
          | otherwise   -> Nothing
        V.EvMouseUp x y Nothing -- Terminal doesn't specify mouse up button,
                                -- assume it's the right one.
          | end       -> Nothing
          | otherwise -> Just $ Drag from (x,y) btn mods True
        _ -> Nothing
  rec let newDrag = attachWithMaybe f (current dragD) inp
      dragD <- holdDyn (Drag (0,0) (0,0) btn [] True) -- gross, but ok because it'll never be produced.
                       newDrag
  return (updated dragD)

mouseDown
  :: (Reflex t, Monad m)
  => V.Button
  -> VtyWidget t m (Event t VtyEvent)
mouseDown btn = do
  i <- input
  return $ fforMaybe i $ \x -> case x of
    V.EvMouseDown _ _ btn' _ -> if btn == btn' then Just x else Nothing
    _ -> Nothing

-- | A plain split of the available space into vertically stacked panes.
-- No visual separator is built in here.
splitV :: (Reflex t, Monad m)
       => Dynamic t (Int -> Int)
       -- ^ Function used to determine size of first pane based on available size
       -> Dynamic t (Bool, Bool)
       -- ^ How to focus the two sub-panes, given that we are focused.
       -> VtyWidget t m a
       -- ^ Widget for first pane
       -> VtyWidget t m b
       -- ^ Widget for second pane
       -> VtyWidget t m (a,b)
splitV sizeFunD focD wA wB = do
  sz <- displaySize
  let regA = (\f (w,h) -> Region 0 0 w (f h)) <$> sizeFunD <*> sz
      regB = (\(w,h) (Region _ _ _ hA) -> Region 0 hA w (h - hA)) <$> sz <*> regA
  ra <- pane regA (fst <$> focD) wA
  rb <- pane regB (snd <$> focD) wB
  return (ra,rb)

-- | A split of the available space into two parts with a draggable separator.
-- Starts with half the space allocated to each, and the first pane has focus.
-- Clicking in a pane switches focus.
splitVDrag :: (Reflex t, MonadFix m, MonadHold t m)
  => VtyWidget t m ()
  -> VtyWidget t m a
  -> VtyWidget t m b
  -> VtyWidget t m (a,b)
splitVDrag wS wA wB = do
  sz <- displaySize
  (_, h0) <- sample $ current sz
  dragE <- drag V.BLeft
  let splitter0 = h0 `div` 2
  rec splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
      splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
      splitterFrac <- holdDyn ((1::Double) / 2) $ ffor (attach (current sz) (fst <$> dragSplitter)) $ \((_,h),x) ->
        fromIntegral x / fromIntegral h
      let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
            \(splitterY, Drag (_, fromY) (_, toY) _ _ end) ->
              if splitterY == fromY then Just (toY, end) else Nothing
          regA = (\(w,_) sp -> Region 0 0 w sp) <$> sz <*> splitterPos
          regS = (\(w,_) sp -> Region 0 sp w 1) <$> sz <*> splitterPos
          regB = (\(w,h) sp -> Region 0 (sp + 1) w (h - sp - 1)) <$> sz <*> splitterPos
          resizeSplitter = ffor (attach (current splitterFrac) (updated sz)) $
            \(frac, (_,h)) -> round (frac * fromIntegral h)
      focA <- holdDyn True $ leftmost
        [ True <$ mA
        , False <$ mB
        ]
      (mA, rA) <- pane regA focA $ withMouseDown wA
      pane regS (pure False) wS
      (mB, rB) <- pane regB (not <$> focA) $ withMouseDown wB
  return (rA, rB)
  where
    withMouseDown x = do
      m <- mouseDown V.BLeft
      x' <- x
      return (m, x')

-- | Fill the background with a particular character.
fill :: (Reflex t, Monad m) => Char -> VtyWidget t m ()
fill c = do
  sz <- displaySize
  let fillImg = ffor (current sz) $ \(w,h) -> [V.charFill V.defAttr c w h]
  tellImages fillImg

-- | Fill the background with the bottom
hRule :: (Reflex t, Monad m) => BoxStyle -> VtyWidget t m ()
hRule boxStyle = fill (_boxStyle_s boxStyle)

fractionSz :: Double -> Int -> Int
fractionSz x h = round (fromIntegral h * x)

modifyImages
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Behavior t ([Image] -> [Image])
  -> VtyWidget t m a
  -> VtyWidget t m a
modifyImages f (VtyWidget w) = VtyWidget $ flip censor w $ \wo ->
  wo { _vtyWidgetOut_images = f <*> (_vtyWidgetOut_images wo) }

data BoxStyle = BoxStyle
  { _boxStyle_nw :: Char
  , _boxStyle_n :: Char
  , _boxStyle_ne :: Char
  , _boxStyle_e :: Char
  , _boxStyle_se :: Char
  , _boxStyle_s :: Char
  , _boxStyle_sw :: Char
  , _boxStyle_w :: Char
  }

instance Default BoxStyle where
  def = singleBoxStyle

hyphenBoxStyle :: BoxStyle
hyphenBoxStyle = BoxStyle '-' '-' '-' '|' '-' '-' '-' '|'

singleBoxStyle :: BoxStyle
singleBoxStyle = BoxStyle '┌' '─' '┐' '│' '┘' '─' '└' '│'

thickBoxStyle :: BoxStyle
thickBoxStyle = BoxStyle '┏' '━' '┓' '┃' '┛' '━' '┗' '┃'

doubleBoxStyle :: BoxStyle
doubleBoxStyle = BoxStyle '╔' '═' '╗' '║' '╝' '═' '╚' '║'

roundedBoxStyle :: BoxStyle
roundedBoxStyle = BoxStyle '╭' '─' '╮' '│' '╯' '─' '╰' '│'

box :: (Monad m, Reflex t)
    => BoxStyle
    -> VtyWidget t m a
    -> VtyWidget t m a
box style child = do
  sz <- displaySize
  let boxReg = ffor (current sz) $ \(w,h) -> Region 0 0 w h
      innerReg = ffor sz $ \(w,h) -> Region 1 1 (w - 2) (h - 2)
  tellImages (fmap boxImages boxReg)
  tellImages (fmap (\r -> [regionBlankImage r]) (current innerReg))
  pane innerReg (pure True) child
  where
    boxImages :: Region -> [Image]
    boxImages (Region left top width height) =
      let right = left + width - 1
          bottom = top + height - 1
          sides =
            [ withinImage (Region (left + 1) top (width - 2) 1) $
                V.charFill V.defAttr (_boxStyle_n style) (width - 2) 1
            , withinImage (Region right (top + 1) 1 (height - 2)) $
                V.charFill V.defAttr (_boxStyle_e style) 1 (height - 2)
            , withinImage (Region (left + 1) bottom (width - 2) 1) $
                V.charFill V.defAttr (_boxStyle_s style) (width - 2) 1
            , withinImage (Region left (top + 1) 1 (height - 2)) $
                V.charFill V.defAttr (_boxStyle_w style) 1 (height - 2)
            ]
          corners =
            [ withinImage (Region left top 1 1) $
                V.char V.defAttr (_boxStyle_nw style)
            , withinImage (Region right top 1 1) $
                V.char V.defAttr (_boxStyle_ne style)
            , withinImage (Region right bottom 1 1) $
                V.char V.defAttr (_boxStyle_se style)
            , withinImage (Region left bottom 1 1) $
                V.char V.defAttr (_boxStyle_sw style)
            ]
      in sides ++ if width > 1 && height > 1 then corners else []

text :: (Reflex t, Monad m) => Behavior t Text -> VtyWidget t m ()
text msg = do
  dw <- displayWidth
  let img = (\w s -> [wrapText w V.defAttr s]) <$> current dw <*> msg
  tellImages img

display :: (Reflex t, Monad m, Show a) => Behavior t a -> VtyWidget t m ()
display a = text $ T.pack . show <$> a

regionBlankImage :: Region -> Image
regionBlankImage r@(Region _ _ width height) =
  withinImage r $ V.charFill V.defAttr ' ' width height

withinImage :: Region -> Image -> Image
withinImage (Region left top width height)
  | width < 0 || height < 0 = withinImage (Region left top 0 0)
  | otherwise = V.translate left top . V.crop width height

wrapText :: Int -> Attr -> Text -> Image
wrapText maxWidth attrs = V.vertCat
  . concatMap (fmap (V.string attrs . T.unpack) . wrapWithOffset maxWidth 0)
  . T.split (=='\n')

wrapWithOffset :: Int -> Int -> Text -> [Text]
wrapWithOffset maxWidth n xs =
  let (firstLine, rest) = T.splitAt (maxWidth - n) xs
  in firstLine : (fmap (T.take maxWidth) . takeWhile (not . T.null) . iterate (T.drop maxWidth) $ rest)

wrapInputState
  :: Int -- ^ Maximum line length
  -> Attr -- ^ Normal string attributes
  -> Attr -- ^ Cursor character attributes
  -> InputState -- ^ The input to render
  -> ([Image], Int)
wrapInputState maxWidth attrs cursorAttrs (InputState before after) =
  let -- Break up the InputState into:
      -- * A top section of lines before the line containing the cursor
      -- * A part of the line containing the cursor that comes immediately before the cursor
      -- * The cursor character (i.e., the character the cursor is on top of)
      -- * The remainder of the line containing the cursor
      -- * The bottom section of lines after the line containing the cursor
      (top, midBefore, cursorChar, midAfter, bottom) = case (initLast before', headTail after') of
        (Nothing, Nothing) -> ([], "", ' ', "", [])
        (Just (top', mid), Nothing) -> (top', mid, ' ', "", [])
        (Nothing, Just (mid, bottom')) -> case T.uncons mid of
          Nothing -> ([], "", ' ', "", bottom')
          Just (c, rest) -> ([], "", c, rest, bottom')
        (Just (top', midB), Just (midA, bottom')) -> case T.uncons midA of
          Nothing -> (top', midB, ' ', "", bottom')
          Just (c, rest) -> (top', midB, c, rest, bottom')
      (midBeforeTop, midBefore') = fromMaybe ([], "") $ initLast $ wrap [midBefore]
      cursorAfterEOL = T.length midBefore' == maxWidth
      (midAfter', midAfterBottom) =
        let offset = if cursorAfterEOL then 1 else T.length midBefore' + 1
        in  case wrapWithOffset maxWidth offset midAfter of
              [] -> ("", [])
              x:xs -> (x, xs)
      cursor = V.char cursorAttrs cursorChar
      images = mconcat
        [ vstring <$> wrap top
        , vstring <$> midBeforeTop
        , if cursorAfterEOL
            then
              [ vstring midBefore'
              , V.horizCat
                [ cursor
                , vstring midAfter'
                ]
              ]
            else
              [ V.horizCat
                [ vstring midBefore'
                , cursor
                , vstring midAfter'
                ]
              ]
        , vstring <$> midAfterBottom
        , vstring <$> bottom
        ]
      cursorY = length top + length midBeforeTop + if cursorAfterEOL then 1 else 0
  in (images, cursorY)
  where
    before' = T.split (=='\n') before
    after' = T.split (=='\n') after
    vstring = V.string attrs . T.unpack
    wrap :: [Text] -> [Text]
    wrap = concatMap (wrapWithOffset maxWidth 0)
    headTail :: [a] -> Maybe (a, [a])
    headTail = \case
      [] -> Nothing
      x:xs -> Just (x, xs)
    initLast :: [a] -> Maybe ([a], a)
    initLast = \case
      [] -> Nothing
      (x:xs) -> case initLast xs of
        Nothing -> Just ([], x)
        Just (ys, y) -> Just (x:ys, y)

data InputState = InputState
  { _inputState_beforeCursor :: Text
  , _inputState_afterCursor :: Text
  }

inputValue :: InputState -> Text
inputValue (InputState a b) = a <> b

updateInputState :: V.Event -> InputState -> InputState
updateInputState ev i@(InputState before after) = case ev of
  -- Regular characters
  V.EvKey (V.KChar k) [] -> InputState (T.snoc before k) after
  -- Deletion buttons
  V.EvKey V.KBS [] -> InputState (if T.null before then T.empty else T.init before) after
  V.EvKey V.KDel [] -> case T.uncons after of
    Nothing -> i
    Just (_, a) -> InputState before a
  -- Key combinations
  V.EvKey (V.KChar 'u') [V.MCtrl] -> InputState mempty mempty
  V.EvKey (V.KChar 'w') [V.MCtrl] -> InputState (T.dropWhileEnd (not . isSpace) $ T.dropWhileEnd isSpace $ before) after
  -- Arrow keys
  V.EvKey V.KRight [] -> case T.uncons after of
    Nothing -> i
    Just (a, as) -> InputState (T.snoc before a) as
  V.EvKey V.KLeft [] -> case T.unsnoc before of
    Nothing -> i
    Just (bs, b) -> InputState bs (T.cons b after)
  V.EvKey V.KUp [] ->
    let (pre, post) = T.breakOnEnd "\n" before
        (top, previousLine) =  T.breakOnEnd "\n" (T.dropEnd 1 pre)
        (previousLineBefore, previousLineAfter) = T.splitAt (T.length post) previousLine
    in if T.null pre
         then i
         else InputState (top <> previousLineBefore) (previousLineAfter <> "\n" <> post <> after)
  V.EvKey V.KDown [] ->
    let offset = T.length $ T.takeWhileEnd (/='\n') before
        (pre, post) = T.breakOn "\n" after
        (nextLine, rest) = T.breakOn "\n" (T.drop 1 post)
        (nextLineBefore, nextLineAfter) = T.splitAt offset nextLine
    in if T.null post
         then i
         else InputState (before <> pre <> "\n" <> nextLineBefore) (nextLineAfter <> rest)
  V.EvKey V.KHome [] ->
    let (pre, post) = T.breakOnEnd "\n" before
    in InputState pre (post <> after)
  V.EvKey V.KEnd [] ->
    let (pre, post) = T.breakOn "\n" after
    in InputState (before <> pre) post
  _ -> i

data TextInputConfig t = TextInputConfig
  { _textInputConfig_initialValue :: Text
  , _textInputConfig_modifyInputState :: Event t (InputState -> InputState)
  }

instance Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig "" never

textInput :: (Reflex t, MonadHold t m, MonadFix m) => TextInputConfig t -> VtyWidget t m (Dynamic t Text)
textInput cfg = do
  i <- input
  v <- foldDyn ($) (InputState (_textInputConfig_initialValue cfg) mempty) $ mergeWith (.)
    [ updateInputState <$> i
    , _textInputConfig_modifyInputState cfg
    ]
  dw <- displayWidth
  dh <- displayHeight
  let img = (\w s -> wrapInputState w V.defAttr (V.withStyle V.defAttr V.reverseVideo) s) -- TODO cursor ought to blink
        <$> dw
        <*> v
  y <- holdUniqDyn $ snd <$> img
  let newScrollTop st (h, cursorY)
        | cursorY < st = cursorY
        | cursorY >= st + h = cursorY - h + 1
        | otherwise = st
  rec let hy = attachWith newScrollTop (current scrollTop) $ updated $ zipDyn dh y
      scrollTop <- holdDyn 0 hy
  tellImages $ (\(imgs, _) st -> (:[]) . V.vertCat $ drop st imgs) <$> current img <*> current scrollTop
  return $ inputValue <$> v

multilineTextInput :: (Reflex t, MonadHold t m, MonadFix m) => TextInputConfig t -> VtyWidget t m (Dynamic t Text)
multilineTextInput cfg = do
  i <- input
  textInput $ cfg
    { _textInputConfig_modifyInputState = mergeWith (.)
      [ fforMaybe i $ \case
          V.EvKey V.KEnter [] -> Just $ \(InputState before after) -> InputState (T.snoc before '\n') after
          _ -> Nothing
      , _textInputConfig_modifyInputState cfg
      ]
    }
