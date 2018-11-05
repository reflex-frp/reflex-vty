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
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Class.Switchable
import Reflex.NotReady.Class
import Reflex.Vty

data Example = Example_TextEditor
             | Example_Todo
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

main :: IO ()
main = mainWidget $ do
  inp <- input
  size <- displaySize
  tellShutdown . fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
  let buttons = do
        text $ pure "Select an example. Esc will bring you back here. Ctrl+c to quit."
        let region1 = ffor size $ \(w,h) ->
              Region (w `div` 6) (h `div` 6) (w `div` 6) (h `div` 6)
            region2 = ffor size $ \(w,h) ->
              Region (2 * w `div` 6) (h `div` 6) (w `div` 6) (h `div` 6)
        todo' <- pane region1 (pure True) $ textButtonStatic def "Todo List"
        editor <- pane region2 (pure True) $ textButtonStatic def "Text Editor"
        return $ leftmost
          [ Left Example_TextEditor <$ editor
          , Left Example_Todo <$ todo'
          ]
      escapable w = do
        w
        i <- input
        return $ fforMaybe i $ \case
          V.EvKey V.KEsc [] -> Just $ Right ()
          _ -> Nothing
  rec out <- networkHold buttons $ ffor (switch (current out)) $ \case
        Left Example_TextEditor -> escapable testBoxes
        Left Example_Todo -> escapable taskList
        Right () -> buttons
  return ()

taskList
  :: (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, NotReady t m, PostBuild t m)
  => VtyWidget t m ()
taskList = do
  let btn = textButtonStatic def "Add another task"
  inp <- input
  rec let todos' = todos [Todo "First" True, Todo "Second" False, Todo "Third" False] $ leftmost
            [ () <$ e
            , fforMaybe inp $ \case
                V.EvKey V.KEnter [] -> Just ()
                _ -> Nothing
            ]
      (m, (e, _)) <- splitV (pure (subtract 6)) (pure (True, True)) todos' $ do
        splitV (pure (subtract 3)) (pure (True, True)) btn (display $ current m)
  return ()

testBoxes :: (Reflex t, MonadHold t m, MonadFix m) => VtyWidget t m ()
testBoxes = do
  size <- displaySize
  let region1 = fmap (\(w,h) -> Region (w `div` 6) (h `div` 6) (w `div` 2) (h `div` 2)) size
      region2 = fmap (\(w,h) -> Region (w `div` 4) (h `div` 4) (2 * (w `div` 3)) (2*(h `div` 3))) size
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

testStringBox :: (Reflex t, Monad m) => VtyWidget t m ()
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
  }

instance Reflex t => Switchable t (TodoOutput t) where
  switching t0 e = TodoOutput
    <$> switching (_todoOutput_todo t0) (_todoOutput_todo <$> e)
    <*> switching (_todoOutput_delete t0) (_todoOutput_delete <$> e)

todo
  :: (MonadHold t m, MonadFix m, Reflex t)
  => Todo
  -> VtyWidget t m (TodoOutput t)
todo t0 = do
  w <- displayWidth
  let checkboxWidth = 3
      checkboxRegion = pure $ Region 0 0 checkboxWidth 1
      labelRegion = ffor w $ \w' -> Region (checkboxWidth + 1) 0 (w' - 1 - checkboxWidth) 1
  value <- pane checkboxRegion (pure True) $ checkbox def $ _todo_done t0
  (label, d) <- pane labelRegion (pure True) $ do
    i <- input
    v <- textInput $ def { _textInputConfig_initialValue = TZ.fromText $ _todo_label t0 }
    let deleteSelf = attachWithMaybe backspaceOnEmpty (current v) i
    return (v, deleteSelf)
  return $ TodoOutput
    { _todoOutput_todo = Todo <$> label <*> value
    , _todoOutput_delete = d
    }
  where
    backspaceOnEmpty v = \case
      V.EvKey V.KBS _ | T.null v -> Just ()
      _ -> Nothing

todos
  :: forall t m. (MonadHold t m, MonadFix m, Reflex t, Adjustable t m, NotReady t m, PostBuild t m)
  => [Todo]
  -> Event t ()
  -> VtyWidget t m (Dynamic t (Map Int Todo))
todos todos0 newTodo = do
  let todosMap0 = Map.fromList $ zip [0..] todos0
  w <- displayWidth
  rec listOut <- listHoldWithKey todosMap0 updates $ \row t -> do
        let reg = zipDynWith (\w' ts ->
              let l = Map.size $ Map.takeWhileAntitone (<row) ts
              in Region 0 l w' 1) w todosMap
        pane reg (fmap (==row) selected) $ do
          e <- mouseUp
          r <- todo t
          return (row <$ e, r)
      let selectionClick = switch . current $ fmap (leftmost . Map.elems . fmap fst) listOut
      selected <- holdDyn 0 $ leftmost
        [ selectionClick
        , fmapMaybe (fmap fst . Map.lookupMax) insert
        , selectOnDelete
        ]
      let todosMap = joinDynThroughMap $ fmap (fmap (_todoOutput_todo . snd)) listOut
          todoDelete = switch . current $
            leftmost .  Map.elems . Map.mapWithKey (\k -> (k <$) . _todoOutput_delete . snd) <$> listOut
          selectOnDelete = attachWithMaybe
            (\m k -> let (before, after) = Map.split k m
                      in  fmap fst $ Map.lookupMax before <|> Map.lookupMin after)
            (current todosMap)
            todoDelete
          insert = ffor (tag (current todosMap) newTodo) $ \m -> case Map.lookupMax m of
            Nothing -> Map.singleton 0 $ Just $ Todo "" False
            Just (k, _) -> Map.union (Just <$> m) $ Map.singleton (k+1) $ Just $ Todo "" False
          delete = ffor (attach (current todosMap) todoDelete) $ \(m, k) ->
            Map.union (Map.singleton k Nothing) $ Just <$> m
          updates = leftmost [insert, delete]
  return todosMap
