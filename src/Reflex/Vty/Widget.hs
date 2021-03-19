{-|
Module: Reflex.Vty.Widget
Description: Basic set of widgets and building blocks for reflex-vty applications
-}
{-# Language UndecidableInstances #-}

module Reflex.Vty.Widget
  ( VtyWidgetCtx(..)
  , VtyWidget(..)
  , VtyWidgetOut(..)
  , ImageWriter(..)
  , runVtyWidget
  , mainWidget
  , mainWidgetWithHandle
  , HasDisplaySize(..)
  , HasFocus(..)
  , HasVtyInput(..)
  , HasVtyWidgetCtx(..)
  , Region(..)
  , regionSize
  , regionBlankImage
  , Drag(..)
  , drag
  , MouseDown(..)
  , MouseUp(..)
  , mouseDown
  , mouseUp
  , ScrollDirection(..)
  , mouseScroll
  , pane
  , splitV
  , splitH
  , splitVDrag
  , boxTitle
  , box
  , boxStatic
  , RichTextConfig(..)
  , richText
  , text
  , scrollableText
  , display
  , BoxStyle(..)
  , hyphenBoxStyle
  , singleBoxStyle
  , roundedBoxStyle
  , thickBoxStyle
  , doubleBoxStyle
  , fill
  , hRule
  , KeyCombo
  , key
  , keys
  , keyCombo
  , keyCombos
  , blank
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.NodeId
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Default (Default(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import Graphics.Vty (Image)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Class ()
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Vty.Host

-- | The context within which a 'VtyWidget' runs
data VtyWidgetCtx t = VtyWidgetCtx
  { _vtyWidgetCtx_width :: Dynamic t Int
    -- ^ The width of the region allocated to the widget.
  , _vtyWidgetCtx_height :: Dynamic t Int
    -- ^ The height of the region allocated to the widget.
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
  { _vtyWidgetOut_shutdown :: Event t ()
  }

instance (Adjustable t m, MonadHold t m, Reflex t) => Adjustable t (VtyWidget t m) where
  runWithReplace a0 a' = VtyWidget $ runWithReplace (unVtyWidget a0) $ fmap unVtyWidget a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' = VtyWidget $
    traverseIntMapWithKeyWithAdjust (\k v -> unVtyWidget (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = VtyWidget $ do
    traverseDMapWithKeyWithAdjust (\k v -> unVtyWidget (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = VtyWidget $ do
    traverseDMapWithKeyWithAdjustWithMove (\k v -> unVtyWidget (f k v)) dm0 dm'

-- | A widget that can read its context and produce image output
newtype VtyWidget t m a = VtyWidget
  { unVtyWidget :: BehaviorWriterT t [Image] (ReaderT (VtyWidgetCtx t) m) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadSample t
    , MonadHold t
    , MonadFix
    , NotReady t
    , ImageWriter t
    , PostBuild t
    , TriggerEvent t
    , MonadReflexCreateTrigger t
    , MonadIO
    )

deriving instance PerformEvent t m => PerformEvent t (VtyWidget t m)
instance MonadTrans (VtyWidget t) where
  lift f = VtyWidget $ lift $ lift f

instance MonadNodeId m => MonadNodeId (VtyWidget t m) where
  getNextNodeId = VtyWidget $ do
    lift $ lift getNextNodeId

class HasVtyWidgetCtx t m | m -> t where
  askCtx :: m (VtyWidgetCtx t)
  default askCtx :: (f m' ~ m, Monad m', MonadTrans f, HasVtyWidgetCtx t m') => m (VtyWidgetCtx t)
  askCtx = lift askCtx
  localCtx :: (VtyWidgetCtx t -> VtyWidgetCtx t) -> (Behavior t [Image] -> Behavior t [Image]) -> m a -> m a

instance (Monad m, Reflex t) => HasVtyWidgetCtx t (VtyWidget t m) where
  askCtx = VtyWidget $ lift ask
  localCtx f g (VtyWidget x) = VtyWidget $ do
    (a, images) :: (a, Behavior t [Image]) <- lift $ local f $ runBehaviorWriterT x
    tellImages $ g images
    pure a

-- | Runs a 'VtyWidget' with a given context
runVtyWidget
  :: (Reflex t, MonadNodeId m)
  => VtyWidgetCtx t
  -> VtyWidget t m a
  -> m (a, Behavior t [Image])
runVtyWidget ctx w = runReaderT (runBehaviorWriterT (unVtyWidget w)) ctx

-- | Sets up the top-level context for a 'VtyWidget' and runs it with that context
mainWidgetWithHandle
  :: V.Vty
  -> (forall t m. (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m (Event t ()))
  -> IO ()
mainWidgetWithHandle vty child =
  runVtyAppWithHandle vty $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let inp' = fforMaybe inp $ \case
          V.EvResize {} -> Nothing
          x -> Just x
    let ctx = VtyWidgetCtx
          { _vtyWidgetCtx_width = fmap fst size
          , _vtyWidgetCtx_height = fmap snd size
          , _vtyWidgetCtx_input = inp'
          , _vtyWidgetCtx_focus = constDyn True
          }
    (shutdown, images) <- runNodeIdT $ runVtyWidget ctx $ do
      tellImages . ffor (current size) $ \(w, h) -> [V.charFill V.defAttr ' ' w h]
      child
    return $ VtyResult
      { _vtyResult_picture = fmap (V.picForLayers . reverse) images
      , _vtyResult_shutdown = shutdown
      }

-- | Like 'mainWidgetWithHandle', but uses a default vty configuration
mainWidget
  :: (forall t m. (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m (Event t ()))
  -> IO ()
mainWidget child = do
  vty <- getDefaultVty
  mainWidgetWithHandle vty child

-- | A class for things that know their own display size dimensions
class (Reflex t, Monad m) => HasDisplaySize t m | m -> t where
  -- | Retrieve the display width (columns)
  displayWidth :: m (Dynamic t Int)
  default displayWidth :: (f m' ~ m, MonadTrans f, HasDisplaySize t m') => m (Dynamic t Int)
  displayWidth = lift displayWidth
  -- | Retrieve the display height (rows)
  displayHeight :: m (Dynamic t Int)
  default displayHeight :: (f m' ~ m, MonadTrans f, HasDisplaySize t m') => m (Dynamic t Int)
  displayHeight = lift displayHeight

instance (Reflex t, Monad m) => HasDisplaySize t (VtyWidget t m) where
  displayWidth = VtyWidget . lift $ asks _vtyWidgetCtx_width
  displayHeight = VtyWidget . lift $ asks _vtyWidgetCtx_height

instance HasDisplaySize t m => HasDisplaySize t (ReaderT x m)
instance HasDisplaySize t m => HasDisplaySize t (BehaviorWriterT t x m)
instance HasDisplaySize t m => HasDisplaySize t (DynamicWriterT t x m)
instance HasDisplaySize t m => HasDisplaySize t (EventWriterT t x m)

instance HasDisplaySize t m => HasDisplaySize t (NodeIdT m)

-- | A class for things that can receive vty events as input
class HasVtyInput t m | m -> t where
  input :: m (Event t VtyEvent)
  default input :: (f m' ~ m, Monad m', MonadTrans f, HasVtyInput t m') => m (Event t VtyEvent)
  input = lift input

instance (Reflex t, Monad m) => HasVtyInput t (VtyWidget t m) where
  input = VtyWidget . lift $ asks _vtyWidgetCtx_input

-- | A class for things that can dynamically gain and lose focus
class HasFocus t m | m -> t where
  focus :: m (Dynamic t Bool)
  default focus :: (f m' ~ m, Monad m', MonadTrans f, HasFocus t m') => m (Dynamic t Bool)
  focus = lift focus

instance (Reflex t, Monad m) => HasFocus t (VtyWidget t m) where
  focus = VtyWidget . lift $ asks _vtyWidgetCtx_focus

-- | A class for widgets that can produce images to draw to the display
class (Reflex t, Monad m) => ImageWriter t m | m -> t where
  -- | Send images upstream for rendering
  tellImages :: Behavior t [Image] -> m ()
  default tellImages :: (f m' ~ m, Monad m', MonadTrans f, ImageWriter t m') => Behavior t [Image] -> m ()
  tellImages = lift . tellImages

instance (Monad m, Reflex t) => ImageWriter t (BehaviorWriterT t [Image] m) where
  tellImages = tellBehavior

-- | A chunk of the display area
data Region = Region
  { _region_left :: Int
  , _region_top :: Int
  , _region_width :: Int
  , _region_height :: Int
  }
  deriving (Show, Read, Eq, Ord)

-- | The width and height of a 'Region'
regionSize :: Region -> (Int, Int)
regionSize (Region _ _ w h) = (w, h)

-- | Produces an 'Image' that fills a region with space characters
regionBlankImage :: Region -> Image
regionBlankImage r@(Region _ _ width height) =
  withinImage r $ V.charFill V.defAttr ' ' width height

-- | Translates and crops an 'Image' so that it is contained by
-- the given 'Region'.
withinImage
  :: Region
  -> Image
  -> Image
withinImage (Region left top width height)
  | width < 0 || height < 0 = withinImage (Region left top 0 0)
  | otherwise = V.translate left top . V.crop width height

-- | Low-level widget combinator that runs a child 'VtyWidget' within
-- a given region and context. This widget filters and modifies the input
-- that the child widget receives such that:
-- * unfocused widgets receive no key events
-- * mouse inputs outside the region are ignored
-- * mouse inputs inside the region have their coordinates translated such
--   that (0,0) is the top-left corner of the region
pane
  :: (Reflex t, Monad m, MonadNodeId m, HasVtyWidgetCtx t m)
  => Dynamic t Region
  -> Dynamic t Bool -- ^ Whether the widget should be focused when the parent is.
  -> m a
  -> m a
pane dr foc child = do
  let reg = current dr
  let subContext ctx = VtyWidgetCtx
        { _vtyWidgetCtx_input = fmapMaybe id $
            attachWith (\(r,f) e -> filterInput r f e)
              (liftA2 (,) reg (current foc))
              (_vtyWidgetCtx_input ctx)
        , _vtyWidgetCtx_focus = liftA2 (&&) (_vtyWidgetCtx_focus ctx) foc
        , _vtyWidgetCtx_width = _region_width <$> dr
        , _vtyWidgetCtx_height = _region_height <$> dr
        }
  let imagesWithinRegion images = liftA2 (\is r -> map (withinImage r) is) images reg
  localCtx subContext imagesWithinRegion child
  where
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

-- | Information about a drag operation
data Drag = Drag
  { _drag_from :: (Int, Int) -- ^ Where the drag began
  , _drag_to :: (Int, Int) -- ^ Where the mouse currently is
  , _drag_button :: V.Button -- ^ Which mouse button is dragging
  , _drag_modifiers :: [V.Modifier] -- ^ What modifiers are held
  , _drag_end :: Bool -- ^ Whether the drag ended (the mouse button was released)
  }
  deriving (Eq, Ord, Show)

-- | Converts raw vty mouse drag events into an event stream of 'Drag's
drag
  :: (Reflex t, MonadFix m, MonadHold t m, HasVtyInput t m)
  => V.Button
  -> m (Event t Drag)
drag btn = do
  inp <- input
  let f :: Maybe Drag -> V.Event -> Maybe Drag
      f Nothing = \case
        V.EvMouseDown x y btn' mods
          | btn == btn' -> Just $ Drag (x,y) (x,y) btn' mods False
          | otherwise   -> Nothing
        _ -> Nothing
      f (Just (Drag from _ _ mods end)) = \case
        V.EvMouseDown x y btn' mods'
          | end && btn == btn'  -> Just $ Drag (x,y) (x,y) btn' mods' False
          | btn == btn'         -> Just $ Drag from (x,y) btn mods' False
          | otherwise           -> Nothing -- Ignore other buttons.
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
      dragD <- holdDyn Nothing $ Just <$> newDrag
  return (fmapMaybe id $ updated dragD)

-- | Mouse down events for a particular mouse button
mouseDown
  :: (Reflex t, Monad m, HasVtyInput t m)
  => V.Button
  -> m (Event t MouseDown)
mouseDown btn = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvMouseDown x y btn' mods -> if btn == btn'
      then Just $ MouseDown btn' (x, y) mods
      else Nothing
    _ -> Nothing

-- | Mouse up events for a particular mouse button
mouseUp
  :: (Reflex t, Monad m, HasVtyInput t m)
  => m (Event t MouseUp)
mouseUp = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvMouseUp x y btn' -> Just $ MouseUp btn' (x, y)
    _ -> Nothing

-- | Information about a mouse down event
data MouseDown = MouseDown
  { _mouseDown_button :: V.Button
  , _mouseDown_coordinates :: (Int, Int)
  , _mouseDown_modifiers :: [V.Modifier]
  }
  deriving (Eq, Ord, Show)

-- | Information about a mouse up event
data MouseUp = MouseUp
  { _mouseUp_button :: Maybe V.Button
  , _mouseUp_coordinates :: (Int, Int)
  }
  deriving (Eq, Ord, Show)

-- | Mouse scroll direction
data ScrollDirection = ScrollDirection_Up | ScrollDirection_Down
  deriving (Eq, Ord, Show)

-- | Produce an event that fires when the mouse wheel is scrolled
mouseScroll
  :: (Reflex t, Monad m, HasVtyInput t m)
  => m (Event t ScrollDirection)
mouseScroll = do
  up <- mouseDown V.BScrollUp
  down <- mouseDown V.BScrollDown
  return $ leftmost
    [ ScrollDirection_Up <$ up
    , ScrollDirection_Down <$ down
    ]

-- | Type synonym for a key and modifier combination
type KeyCombo = (V.Key, [V.Modifier])

-- | Emits an event that fires on a particular key press (without modifiers)
key :: (Monad m, Reflex t, HasVtyInput t m) => V.Key -> m (Event t KeyCombo)
key = keyCombos . Set.singleton . (,[])

-- | Emits an event that fires on particular key presses (without modifiers)
keys :: (Monad m, Reflex t, HasVtyInput t m) => [V.Key] -> m (Event t KeyCombo)
keys = keyCombos . Set.fromList . fmap (,[])

-- | Emit an event that fires whenever the provided key combination occurs
keyCombo
  :: (Reflex t, Monad m, HasVtyInput t m)
  => KeyCombo
  -> m (Event t KeyCombo)
keyCombo = keyCombos . Set.singleton

-- | Emit an event that fires whenever any of the provided key combinations occur
keyCombos
  :: (Reflex t, Monad m, HasVtyInput t m)
  => Set KeyCombo
  -> m (Event t KeyCombo)
keyCombos ks = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvKey k m -> if Set.member (k, m) ks
      then Just (k, m)
      else Nothing
    _ -> Nothing

-- | A plain split of the available space into vertically stacked panes.
-- No visual separator is built in here.
splitV :: (Reflex t, Monad m, MonadNodeId m, HasVtyWidgetCtx t m, HasDisplaySize t m)
       => Dynamic t (Int -> Int)
       -- ^ Function used to determine size of first pane based on available size
       -> Dynamic t (Bool, Bool)
       -- ^ How to focus the two sub-panes, given that we are focused.
       -> m a
       -- ^ Widget for first pane
       -> m b
       -- ^ Widget for second pane
       -> m (a,b)
splitV sizeFunD focD wA wB = do
  dw <- displayWidth
  dh <- displayHeight
  let regA = Region 0 0 <$> dw <*> (sizeFunD <*> dh)
      regB = Region 0 <$> (_region_height <$> regA) <*> dw <*> liftA2 (-) dh (_region_height <$> regA)
  ra <- pane regA (fst <$> focD) wA
  rb <- pane regB (snd <$> focD) wB
  return (ra,rb)

-- | A plain split of the available space into horizontally stacked panes.
-- No visual separator is built in here.
splitH :: (Reflex t, Monad m, MonadNodeId m, HasDisplaySize t m, HasVtyWidgetCtx t m)
       => Dynamic t (Int -> Int)
       -- ^ Function used to determine size of first pane based on available size
       -> Dynamic t (Bool, Bool)
       -- ^ How to focus the two sub-panes, given that we are focused.
       -> m a
       -- ^ Widget for first pane
       -> m b
       -- ^ Widget for second pane
       -> m (a,b)
splitH sizeFunD focD wA wB = do
  dw <- displayWidth
  dh <- displayHeight
  let regA = Region 0 0 <$> (sizeFunD <*> dw) <*> dh
      regB = Region <$> (_region_width <$> regA) <*> 0 <*> liftA2 (-) dw (_region_width <$> regA) <*> dh
  liftA2 (,) (pane regA (fmap fst focD) wA) (pane regB (fmap snd focD) wB)

-- | A split of the available space into two parts with a draggable separator.
-- Starts with half the space allocated to each, and the first pane has focus.
-- Clicking in a pane switches focus.
splitVDrag :: (Reflex t, MonadFix m, MonadHold t m, MonadNodeId m, HasDisplaySize t m, HasVtyInput t m, HasVtyWidgetCtx t m)
  => m ()
  -> m a
  -> m b
  -> m (a,b)
splitVDrag wS wA wB = do
  dh <- displayHeight
  dw <- displayWidth
  h0 <- sample $ current dh -- TODO
  dragE <- drag V.BLeft
  let splitter0 = h0 `div` 2
  rec splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
      splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
      splitterFrac <- holdDyn ((1::Double) / 2) $ ffor (attach (current dh) (fst <$> dragSplitter)) $ \(h, x) ->
        fromIntegral x / max 1 (fromIntegral h)
      let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
            \(splitterY, Drag (_, fromY) (_, toY) _ _ end) ->
              if splitterY == fromY then Just (toY, end) else Nothing
          regA = Region 0 0 <$> dw <*> splitterPos
          regS = Region 0 <$> splitterPos <*> dw <*> 1
          regB = Region 0 <$> (splitterPos + 1) <*> dw <*> (dh - splitterPos - 1)
          resizeSplitter = ffor (attach (current splitterFrac) (updated dh)) $
            \(frac, h) -> round (frac * fromIntegral h)
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
fill :: (HasDisplaySize t m, ImageWriter t m) => Char -> m ()
fill c = do
  dw <- displayWidth
  dh <- displayHeight
  let fillImg = current $ liftA2 (\w h -> [V.charFill V.defAttr c w h]) dw dh
  tellImages fillImg

-- | Fill the background with the bottom
hRule :: (HasDisplaySize t m, ImageWriter t m) => BoxStyle -> m ()
hRule boxStyle = fill (_boxStyle_s boxStyle)

-- | Defines a set of symbols to use to draw the outlines of boxes
-- C.f. https://en.wikipedia.org/wiki/Box-drawing_character
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

-- | A box style that uses hyphens and pipe characters. Doesn't handle
-- corners very well.
hyphenBoxStyle :: BoxStyle
hyphenBoxStyle = BoxStyle '-' '-' '-' '|' '-' '-' '-' '|'

-- | A single line box style
singleBoxStyle :: BoxStyle
singleBoxStyle = BoxStyle '┌' '─' '┐' '│' '┘' '─' '└' '│'

-- | A thick single line box style
thickBoxStyle :: BoxStyle
thickBoxStyle = BoxStyle '┏' '━' '┓' '┃' '┛' '━' '┗' '┃'

-- | A double line box style
doubleBoxStyle :: BoxStyle
doubleBoxStyle = BoxStyle '╔' '═' '╗' '║' '╝' '═' '╚' '║'

-- | A single line box style with rounded corners
roundedBoxStyle :: BoxStyle
roundedBoxStyle = BoxStyle '╭' '─' '╮' '│' '╯' '─' '╰' '│'

-- | Draws a titled box in the provided style and a child widget inside of that box
boxTitle :: (Monad m, Reflex t, MonadNodeId m, HasDisplaySize t m, ImageWriter t m, HasVtyWidgetCtx t m)
    => Behavior t BoxStyle
    -> Text
    -> m a
    -> m a
boxTitle boxStyle title child = do
  dh <- displayHeight
  dw <- displayWidth
  let boxReg = Region 0 0 <$> dw <*> dh
      innerReg = Region 1 1 <$> (subtract 2 <$> dw) <*> (subtract 2 <$> dh)
  tellImages (boxImages <$> boxStyle <*> current boxReg)
  tellImages (fmap (\r -> [regionBlankImage r]) (current innerReg))
  pane innerReg (pure True) child
  where
    boxImages :: BoxStyle -> Region -> [Image]
    boxImages style (Region left top width height) =
      let right = left + width - 1
          bottom = top + height - 1
          sides =
            [ withinImage (Region (left + 1) top (width - 2) 1) $
                V.text' V.defAttr $
                  hPadText title (_boxStyle_n style) (width - 2)
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
    hPadText :: T.Text -> Char -> Int -> T.Text
    hPadText t c l = if lt >= l
                     then t
                     else left <> t <> right
      where
        lt = T.length t
        delta = l - lt
        mkHalf n = T.replicate (n `div` 2) (T.singleton c)
        left = mkHalf $ delta + 1
        right = mkHalf delta

-- | A box without a title
box :: (Monad m, Reflex t, MonadNodeId m, HasDisplaySize t m, ImageWriter t m, HasVtyWidgetCtx t m)
    => Behavior t BoxStyle
    -> m a
    -> m a
box boxStyle = boxTitle boxStyle mempty

-- | A box whose style is static
boxStatic
  :: (Monad m, Reflex t, MonadNodeId m, HasDisplaySize t m, ImageWriter t m, HasVtyWidgetCtx t m)
  => BoxStyle
  -> m a
  -> m a
boxStatic = box . pure

-- | Configuration options for displaying "rich" text
data RichTextConfig t = RichTextConfig
  { _richTextConfig_attributes :: Behavior t V.Attr
  }

instance Reflex t => Default (RichTextConfig t) where
  def = RichTextConfig $ pure V.defAttr

-- | A widget that displays text with custom time-varying attributes
richText
  :: (Reflex t, Monad m, HasDisplaySize t m, ImageWriter t m)
  => RichTextConfig t
  -> Behavior t Text
  -> m ()
richText cfg t = do
  dw <- displayWidth
  let img = (\w a s -> [wrapText w a s])
        <$> current dw
        <*> _richTextConfig_attributes cfg
        <*> t
  tellImages img
  where
    wrapText maxWidth attrs = V.vertCat
      . concatMap (fmap (V.string attrs . T.unpack) . TZ.wrapWithOffset maxWidth 0)
      . T.split (=='\n')

-- | Renders text, wrapped to the container width
text
  :: (Reflex t, Monad m, HasDisplaySize t m, ImageWriter t m)
  => Behavior t Text
  -> m ()
text = richText def

-- | Scrollable text widget. The output pair exposes the current scroll position and total number of lines (including those
-- that are hidden)
scrollableText
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m, HasDisplaySize t m, HasVtyInput t m, ImageWriter t m)
  => Event t Int
  -- ^ Number of lines to scroll by
  -> Behavior t Text
  -> m (Behavior t (Int, Int))
  -- ^ (Current scroll position, total number of lines)
scrollableText scrollBy t = do
  dw <- displayWidth
  let imgs = wrap <$> current dw <*> t
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t Int
      requestedScroll = leftmost
        [ 1 <$ kdown
        , (-1) <$ kup
        , ffor m $ \case
            ScrollDirection_Up -> (-1)
            ScrollDirection_Down -> 1
        , scrollBy
        ]
      updateLine maxN delta ix = min (max 0 (ix + delta)) maxN
  lineIndex :: Dynamic t Int <- foldDyn (\(maxN, delta) ix -> updateLine (maxN - 1) delta ix) 0 $
    attach (length <$> imgs) requestedScroll
  tellImages $ fmap ((:[]) . V.vertCat) $ drop <$> current lineIndex <*> imgs
  return $ (,) <$> ((+) <$> current lineIndex <*> pure 1) <*> (length <$> imgs)
  where
    wrap maxWidth = concatMap (fmap (V.string V.defAttr . T.unpack) . TZ.wrapWithOffset maxWidth 0) . T.split (=='\n')

-- | Renders any behavior whose value can be converted to
-- 'String' as text
display
  :: (Reflex t, Monad m, Show a, HasDisplaySize t m, ImageWriter t m)
  => Behavior t a
  -> m ()
display a = text $ T.pack . show <$> a

-- | A widget that draws nothing
blank :: Monad m => m ()
blank = return ()
