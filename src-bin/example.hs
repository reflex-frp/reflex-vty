{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -threaded #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.NodeId
import Data.Functor.Misc
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Class.Switchable
import Reflex.Vty

data Example = Example_TextEditor
             | Example_Todo
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

div' :: (Integral a, Applicative f) => f a -> f a -> f a
div' = liftA2 div

main :: IO ()
main = mainWidget $ do
  inp <- input
  let buttons = col $ do
        fixed 4 $ col $ do
          fixed 1 $ text "Select an example."
          fixed 1 $ text "Esc will bring you back here."
          fixed 1 $ text "Ctrl+c to quit."
        a <- fixed 5 $ textButtonStatic def "Todo List"
        b <- fixed 5 $ textButtonStatic def "Text Editor"
        return $ leftmost
          [ Left Example_Todo <$ a
          , Left Example_TextEditor <$ b
          ]
      escapable w = do
        void w
        i <- input
        return $ fforMaybe i $ \case
          V.EvKey V.KEsc [] -> Just $ Right ()
          _ -> Nothing
  rec out <- networkHold buttons $ ffor (switch (current out)) $ \case
        Left Example_TextEditor -> escapable testBoxes
        Left Example_Todo -> escapable taskList
        Right () -> buttons
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

taskList
  :: (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, NotReady t m, PostBuild t m, MonadNodeId m)
  => VtyWidget t m ()
taskList = do
  let btn = textButtonStatic def "Add another task"
  inp <- input
  rec let todos' = todos [Todo "Find reflex-vty" True, Todo "Become functional reactive" False, Todo "Make vty apps" False] $ leftmost
            [ () <$ e
            , fforMaybe inp $ \case
                V.EvKey V.KEnter [] -> Just ()
                _ -> Nothing
            ]
      (m, (e, _)) <- splitV (pure (subtract 6)) (pure (True, True)) todos' $ do
        splitV (pure (subtract 3)) (pure (True, True)) btn (display $ Map.size <$> current m)
  return ()

testBoxes
  :: (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m ()
testBoxes = do
  dw <- displayWidth
  dh <- displayHeight
  let region1 = DynRegion (div' dw 6) (div' dh 6) (div' dw 2) (div' dh 2)
      region2 = DynRegion (div' dw 4) (div' dh 4) (2 * div' dw 3) (2 * div' dh 3)
  pane region1 (constDyn False) . boxStatic singleBoxStyle $ debugInput
  _ <- pane region2 (constDyn True) . boxStatic singleBoxStyle $
    splitVDrag (hRule doubleBoxStyle) (boxStatic roundedBoxStyle $ multilineTextInput def) (boxStatic roundedBoxStyle dragTest)
  return ()

debugFocus :: (Reflex t, Monad m) => VtyWidget t m ()
debugFocus = do
  f <- focus
  text $ T.pack . show <$> current f

debugInput :: (Reflex t, MonadHold t m) => VtyWidget t m ()
debugInput = do
  lastEvent <- hold "No event yet" . fmap show =<< input
  text $ T.pack <$> lastEvent

dragTest :: (Reflex t, MonadHold t m, MonadFix m) => VtyWidget t m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  text $ T.pack <$> lastEvent

testStringBox :: (Reflex t, Monad m, MonadNodeId m) => VtyWidget t m ()
testStringBox = boxStatic singleBoxStyle .
  text . pure . T.pack . take 500 $ cycle ('\n' : ['a'..'z'])

data Todo = Todo
  { _todo_label :: Text
  , _todo_done :: Bool
  }
  deriving (Show, Read, Eq, Ord)

data TodoOutput t = TodoOutput
  { _todoOutput_todo :: Dynamic t Todo
  , _todoOutput_delete :: Event t ()
  , _todoOutput_height :: Dynamic t Int
  }

instance Reflex t => Switchable t (TodoOutput t) where
  switching t0 e = TodoOutput
    <$> switching (_todoOutput_todo t0) (_todoOutput_todo <$> e)
    <*> switching (_todoOutput_delete t0) (_todoOutput_delete <$> e)
    <*> switching (_todoOutput_height t0) (_todoOutput_height <$> e)

todo
  :: (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => Todo
  -> VtyWidget t m (TodoOutput t)
todo t0 = do
  w <- displayWidth
  rec let checkboxWidth = 3
          checkboxRegion = DynRegion 0 0 checkboxWidth 1
          labelHeight = _textInput_lines ti
          labelWidth = w - 1 - checkboxWidth
          labelLeft = checkboxWidth + 1 
          labelTop = constDyn 0
          labelRegion = DynRegion labelLeft labelTop labelWidth labelHeight
      value <- pane checkboxRegion (pure True) $ checkbox def $ _todo_done t0
      (ti, d) <- pane labelRegion (pure True) $ do
        i <- input
        v <- textInput $ def { _textInputConfig_initialValue = TZ.fromText $ _todo_label t0 }
        let deleteSelf = attachWithMaybe backspaceOnEmpty (current $ _textInput_value v) i
        return (v, deleteSelf)
  return $ TodoOutput
    { _todoOutput_todo = Todo <$> (_textInput_value ti) <*> value
    , _todoOutput_delete = d
    , _todoOutput_height = _textInput_lines ti
    }
  where
    backspaceOnEmpty v = \case
      V.EvKey V.KBS _ | T.null v -> Just ()
      _ -> Nothing

todos
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, Adjustable t m, NotReady t m, PostBuild t m, MonadNodeId m)
  => [Todo]
  -> Event t ()
  -> VtyWidget t m (Dynamic t (Map Int (TodoOutput t)))
todos todos0 newTodo = do
  let todosMap0 = Map.fromList $ zip [0..] todos0
  rec tabNav <- tabNavigation
      let insertNav = 1 <$ insert
          nav = leftmost [tabNav, insertNav]
      listOut <- runLayout (pure Orientation_Column) 0 nav $
        listHoldWithKey todosMap0 updates $ \k t -> tile (pure $ Constraint_Fixed 1) (pure True) $ do
          let sel = select selectOnDelete $ Const2 k
          click <- fmap (const ()) <$> mouseDown V.BLeft
          pb <- getPostBuild
          let focusMe = leftmost [ click, sel, pb ]
          r <- todo t
          return (focusMe, r)
      let delete = ffor todoDelete $ \k -> Map.singleton k Nothing
          updates = leftmost [insert, delete]
          todoDelete = switch . current $
            leftmost .  Map.elems . Map.mapWithKey (\k -> (k <$) . _todoOutput_delete) <$> listOut
          todosMap = joinDynThroughMap $ fmap (_todoOutput_todo) <$> listOut
          insert = ffor (tag (current todosMap) newTodo) $ \m -> case Map.lookupMax m of
            Nothing -> Map.singleton 0 $ Just $ Todo "" False
            Just (k, _) -> Map.singleton (k+1) $ Just $ Todo "" False
          selectOnDelete = fanMap $ fmap (flip Map.singleton ()) $ attachWithMaybe
            (\m k -> let (before, after) = Map.split k m
                      in  fmap fst $ Map.lookupMax before <|> Map.lookupMin after)
            (current todosMap)
            todoDelete
  return listOut
