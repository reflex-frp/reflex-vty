{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}

{-# OPTIONS_GHC -Wall #-}

module Reflex.Vty
  ( VtyApp
  , VtyResult(..)
  , VtyEvent
  , runVtyApp
  , runVtyAppWith
  , HasDisplaySize(..)
  , displayWidth
  , displayHeight
  , HasFocus(..)
  , HasVtyInput(..)
  , Region(..)
  , regionSize
  , regionBlankImage
  , boxImages
  , runVtyWidget
  , VtyWidgetCtx(..)
  , VtyWidget(..)
  , VtyWidgetOut(..)
  , Drag (..)
  , drag
  , pane
  , modifyImages
  , mainVtyWidget
  , filterInput
  , tellImages
  , tellShutdown
  , wrapString
  , splitV
  , fractionSz
  , box
  , string
  ) where

import Control.Applicative
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (forM, forM_, forever)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref, readRef)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef)
import Data.IORef (readIORef)
import Data.Maybe (catMaybes)

import Reflex
import Reflex.Host.Class
import Reflex.NotReady.Class
import Reflex.Spider.Orphans ()
import qualified Graphics.Vty as V
import Graphics.Vty (DisplayRegion, Image, Attr)

type VtyEvent = V.Event

-- | The output of a 'VtyApp'.
data VtyResult t = VtyResult
  { _vtyResult_picture :: Behavior t V.Picture
  -- ^ The current vty output. 'runVtyAppWith' samples this value every time an
  -- event fires and updates the display.
  , _vtyResult_shutdown :: Event t ()
  -- ^ An event that requests application termination.
  }

type MonadVtyApp t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , PrimMonad (HostFrame t)
  , ReflexHost t
  , MonadIO (HostFrame t)
  , Ref m ~ IORef
  , Ref (HostFrame t) ~ IORef
  , MonadRef (HostFrame t)
  , NotReady t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  )

-- | A functional reactive vty application.
type VtyApp t m = MonadVtyApp t m
  => DisplayRegion
  -- ^ The initial display size (updates to this come as events)
  -> Event t (V.Event)
  -- ^ Vty input events.
  -> m (VtyResult t)
  -- ^ The output of the 'VtyApp'. The application runs in a context that,
  -- among other things, allows new events to be created and triggered
  -- ('TriggerEvent'), provides access to an event that fires immediately upon
  -- app instantiation ('PostBuild'), and allows actions to be run upon
  -- occurrences of events ('PerformEvent').

-- | Runs a 'VtyApp' in a given 'Vty'.
runVtyAppWith
  :: V.Vty
  -- ^ A 'Vty' handle.
  -> (forall t m. VtyApp t m)
  -- ^ A functional reactive vty application.
  -> IO ()
runVtyAppWith vty vtyGuest =
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline. The scoped type signature
  -- specifies that our host runs on the Global timeline.
  -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.
  (runSpiderHost :: SpiderHost Global a -> IO a) $ do

    -- Create an 'Event' and a "trigger" reference for that event. The trigger
    -- reference can be used to determine whether anyone is "subscribed" to
    -- that 'Event' and, therefore, whether we need to bother performing any
    -- updates when the 'Event' fires.
    -- The 'Event' below will be used to convey vty input events.
    (vtyEvent, vtyEventTriggerRef) <- newEventWithTriggerRef

    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newChan

    displayRegion0 <- V.displayBounds $ V.outputIface vty

    -- Run the vty "guest" application, providing the appropriate context. The
    -- result is a 'VtyResult', and a 'FireCommand' that will be used to
    -- trigger events.
    (vtyResult, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the guest app to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventT events $  -- Allows the guest app to create new
                                          -- events and triggers and writes
                                          -- those triggers to a channel from
                                          -- which they will be read and
                                          -- processed.

            vtyGuest displayRegion0 vtyEvent
                                          -- The guest app is provided the
                                          -- initial display region and an
                                          -- 'Event' of vty inputs.

    -- Reads the current value of the 'Picture' behavior and updates the
    -- display with it. This will be called whenever we determine that a
    -- display update is necessary. In this implementation that is when various
    -- events occur.
    let updateVty =
          sample (_vtyResult_picture vtyResult) >>= liftIO . V.update vty

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- When there is a subscriber to the post-build event, fire the event.
    forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()

    -- After firing the post-build event, sample the vty result and update
    -- the display.
    updateVty

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent $ _vtyResult_shutdown vtyResult

    -- Fork a thread and continuously get the next vty input event, and then
    -- write the input event to our channel of FRP 'Event' triggers.
    -- The thread is forked here because 'nextEvent' blocks.
    nextEventThread <- liftIO $ forkIO $ forever $ do
      -- Retrieve the next input event.
      ne <- V.nextEvent vty
      let -- The reference to the vty input 'EventTrigger'. This is the trigger
          -- we'd like to associate the input event value with.
          triggerRef = EventTriggerRef vtyEventTriggerRef
          -- Create an event 'TriggerInvocation' with the value that we'd like
          -- the event to have if it is fired. It may not fire with this value
          -- if nobody is subscribed to the 'Event'.
          triggerInvocation = TriggerInvocation ne $ return ()
      -- Write our input event's 'EventTrigger' with the newly created
      -- 'TriggerInvocation' value to the queue of events.
      writeChan events $ [triggerRef :=> triggerInvocation]

    -- The main application loop. We wait for new events, fire those that
    -- have subscribers, and update the display. If we detect a shutdown
    -- request, the application terminates.
    fix $ \loop -> do
      -- Read the next event (blocking).
      ers <- liftIO $ readChan events
      stop <- do
        -- Fire events that have subscribers.
        fireEventTriggerRefs fc ers $
          -- Check if the shutdown 'Event' is firing.
          readEvent shutdown >>= \case
            Nothing -> return False
            Just _ -> return True
      if or stop
        then liftIO $ do             -- If we received a shutdown 'Event'
          killThread nextEventThread -- then stop reading input events and
          V.shutdown vty             -- call the 'Vty's shutdown command.

        else do                      -- Otherwise, update the display and loop.
          updateVty
          loop
  where
    -- TODO Some part of this is probably general enough to belong in reflex
    -- | Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: (Monad (ReadPhase m), MonadIO m)
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          return $ fmap (\e -> e :=> Identity a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return a

-- | Run a 'VtyApp' with a 'Vty' handle with a standard configuration.
runVtyApp
  :: (forall t m. VtyApp t m)
  -> IO ()
runVtyApp app = do
  cfg <- liftIO V.standardIOConfig
  vty <- liftIO $ V.mkVty $ cfg { V.mouseMode = Just True }
  runVtyAppWith vty app


---- Widget.hs

data Region = Region
  { _region_left :: Int
  , _region_top :: Int
  , _region_width :: Int
  , _region_height :: Int
  }
  deriving (Show, Read, Eq, Ord)

regionSize :: Region -> (Int, Int)
regionSize (Region _ _ w h) = (w, h)

regionBlankImage :: Region -> Image
regionBlankImage r@(Region _ _ width height) =
  withinImage r $ wrapString width V.defAttr $ replicate (width * height) ' '

withinImage :: Region -> Image -> Image
withinImage (Region left top width height)
  | width < 0 || height < 0 = withinImage (Region left top 0 0)
  | otherwise = V.translate left top . V.crop width height

wrapString :: Int -> Attr -> String -> Image
wrapString maxWidth attrs = V.vertCat . concatMap (fmap (V.string attrs) . fmap (take maxWidth) . takeWhile (not . null) . iterate (drop maxWidth)) . lines

boxImages :: Region -> [Image]
boxImages r@(Region left top width height) =
  let hBorder = V.string mempty $ replicate width '-'
      vBorder = wrapString 1 mempty $ replicate (height - 2) '|'
  in  [ withinImage (r { _region_height = 1 }) hBorder
      , withinImage (Region left (top + 1) 1 (height - 2)) vBorder
      , withinImage (Region (left + width - 1) (top + 1) 1 (height - 2)) vBorder
      , withinImage (r { _region_top = top + height - 1 }) hBorder
      ]

class (Reflex t, Monad m) => HasDisplaySize t m | m -> t where
  displaySize :: m (Dynamic t (Int, Int))

displayWidth :: HasDisplaySize t m => m (Dynamic t Int)
displayWidth = fmap fst <$> displaySize

displayHeight :: HasDisplaySize t m => m (Dynamic t Int)
displayHeight = fmap snd <$> displaySize

class HasVtyInput t m | m -> t where
  input :: m (Event t VtyEvent)

class HasFocus t m | m -> t where
  focus :: m (Dynamic t Bool)

data VtyWidgetCtx t = VtyWidgetCtx
  { _vtyWidgetCtx_size :: Dynamic t (Int,Int) -- ^ The width and height of the region allocated to the widget.
  , _vtyWidgetCtx_focus :: Dynamic t Bool -- ^ Whether the widget should behave as if it has focus for keyboard input.
  , _vtyWidgetCtx_input :: Event t VtyEvent -- ^ User input events that the widget's parent chooses to share. These will generally
                                            -- be filtered for relevance: 
                                            --  * Keyboard inputs are restricted to focused widgets
                                            --  * Mouse inputs are restricted to the region in which the widget resides.
  }

data VtyWidgetOut t = VtyWidgetOut
  { _vtyWidgetOut_images :: Behavior t [Image]
  , _vtyWidgetOut_shutdown :: Event t ()
  }

instance (Reflex t) => Semigroup (VtyWidgetOut t) where
  wo <> wo' = VtyWidgetOut
    { _vtyWidgetOut_images = _vtyWidgetOut_images wo <> _vtyWidgetOut_images wo'
    , _vtyWidgetOut_shutdown = _vtyWidgetOut_shutdown wo <> _vtyWidgetOut_shutdown wo'
    }

instance (Reflex t) => Monoid (VtyWidgetOut t) where
  mempty = VtyWidgetOut mempty mempty
  mappend wo wo' = wo <> wo'

newtype VtyWidget t m a = VtyWidget { unVtyWidget :: WriterT (VtyWidgetOut t) (ReaderT (VtyWidgetCtx t) m) a }
  deriving (Functor, Applicative, Monad, MonadSample t, MonadHold t, MonadFix)

runVtyWidget :: (Reflex t)
  => VtyWidgetCtx t
  -> VtyWidget t m a
  -> m (a, VtyWidgetOut t)
runVtyWidget ctx w = runReaderT (runWriterT (unVtyWidget w)) ctx

mainVtyWidget :: (forall t m. MonadVtyApp t m => VtyWidget t m ()) -> IO ()
mainVtyWidget child =
  runVtyApp $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let ctx = VtyWidgetCtx
          { _vtyWidgetCtx_size = size
          , _vtyWidgetCtx_input = inp
          , _vtyWidgetCtx_focus = constDyn True
          }
    ((), wo) <- runVtyWidget ctx child
    return $ VtyResult
      { _vtyResult_picture = fmap (V.picForLayers . reverse) (_vtyWidgetOut_images wo)
      , _vtyResult_shutdown = _vtyWidgetOut_shutdown wo
      }

modifyImages :: (Reflex t, MonadHold t m, MonadFix m)
  => Behavior t ([Image] -> [Image]) -> VtyWidget t m a -> VtyWidget t m a
modifyImages f (VtyWidget w) = VtyWidget $
  censor (\wo -> wo { _vtyWidgetOut_images = f <*> (_vtyWidgetOut_images wo) })
         w

instance (Reflex t, Monad m) => HasDisplaySize t (VtyWidget t m) where
  displaySize = VtyWidget . lift $ asks _vtyWidgetCtx_size

instance (Reflex t, Monad m) => HasVtyInput t (VtyWidget t m) where
  input = VtyWidget . lift $ asks _vtyWidgetCtx_input

instance (Reflex t, Monad m) => HasFocus t (VtyWidget t m) where
  focus = VtyWidget . lift $ asks _vtyWidgetCtx_focus

tellImages :: (Reflex t, Monad m) => Behavior t [Image] -> VtyWidget t m ()
tellImages imgs = VtyWidget $ tell (mempty { _vtyWidgetOut_images = imgs })

tellShutdown :: (Reflex t, Monad m) => Event t () -> VtyWidget t m ()
tellShutdown sd = VtyWidget $ tell (mempty { _vtyWidgetOut_shutdown = sd })

data Drag = Drag
  { _drag_from :: (Int, Int) -- ^ Where the drag began
  , _drag_to :: (Int, Int) -- ^ Where the mouse currently is
  , _drag_button :: V.Button -- ^ Which mouse button is dragging
  , _drag_modifiers :: [V.Modifier] -- ^ What modifiers are held
  , _drag_end :: Bool -- ^ Whether the drag ended (the mouse button was released)
  }
  deriving (Eq, Ord, Show)

drag :: (Reflex t, MonadFix m, MonadHold t m) => V.Button -> VtyWidget t m (Event t Drag)
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
      dragD <- holdDyn (Drag (0,0) (0,0) V.BLeft [] True) -- gross, but ok.
                       newDrag
  return (updated dragD)

pane :: (Reflex t, Monad m)
     => Dynamic t Region -- ^ Region into which we should draw the widget (in coordinates relative to our own)
     -> Dynamic t Bool -- ^ Whether the widget should be focused when the parent is.
     -> VtyWidget t m a
     -> VtyWidget t m a
pane reg foc child = VtyWidget $ do
  ctx <- lift ask
  let ctx' = VtyWidgetCtx
        { _vtyWidgetCtx_input = leftmost -- TODO: think about this leftmost more.
            [ ffor (updated reg) $ \(Region _ _ w h) -> V.EvResize w h
            , fmapMaybe id $
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
{- 
splitVDrag :: (Reflex t, Monad m)
  => VtyWidget t m a
  -> VtyWidget t m b
  -> VtyWidget t m (a,b)
splitVDrag wA wB = do
  sz <- displaySize
  let splitterPos = ffor sz $ \(_,h) -> h `div` 2
      regA = (\(w,_) sp -> Region 0 0 w sp) <$> sz <*> splitterPos
      regS = (\(w,h) sp -> Region 0 sp w 1) <$> sz <*> splitterPos
      regB = (\(w,h) sp -> Region 0 (sp + 1) w (h - sp - 1)) <$> sz <*> splitterPos
  foc <- holdDyn False []
  (rA, inpA) <- pane wA 
-}

fractionSz :: Double -> Int -> Int
fractionSz x h = round (fromIntegral h * x)

box :: (Monad m, Reflex t)
    => VtyWidget t m a
    -> VtyWidget t m a
box child = do
  sz <- displaySize
  let boxReg = ffor (current sz) $ \(w,h) -> Region 0 0 w h
      innerReg = ffor sz $ \(w,h) -> Region 1 1 (w - 2) (h - 2)
  tellImages (fmap boxImages boxReg)
  tellImages (fmap (\r -> [regionBlankImage r]) (current innerReg))
  pane innerReg (pure True) child

string :: (Reflex t, Monad m) => Behavior t String -> VtyWidget t m ()  
string msg = do
  dw <- displayWidth
  let img = (\w s -> [wrapString w mempty s]) <$> current dw <*> msg
  tellImages img