{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -threaded #-}

import Control.Monad.Fix
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.NotReady.Class
import Reflex.Vty

main :: IO ()
main = mainWidget $ do
  inp <- input
  tellShutdown . fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
  let btn = button $ pure "Add another task"
  rec let todos'' = todos' [Todo "First" True, Todo "Second" False, Todo "Third" False] $ () <$ e
      (m, (e, _)) <- splitV (pure (subtract 6)) (pure (True, True)) todos'' $ do
        splitV (pure (subtract 3)) (pure (True, True)) btn (display $ current m)
  return ()

testBoxes :: (Reflex t, MonadHold t m, MonadFix m) => VtyWidget t m ()
testBoxes = do 
  size <- displaySize
  let region1 = fmap (\(w,h) -> Region (w `div` 6) (h `div` 6) (w `div` 2) (h `div` 2)) size
      region2 = fmap (\(w,h) -> Region (w `div` 4) (h `div` 4) (2 * (w `div` 3)) (2*(h `div` 3))) size
  pane region1 (constDyn False) . box singleBoxStyle $ debugInput
  _ <- pane region2 (constDyn True) . box singleBoxStyle $
    splitVDrag (hRule doubleBoxStyle) (box roundedBoxStyle $ multilineTextInput def) (box roundedBoxStyle dragTest)
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
testStringBox = box singleBoxStyle .
  text . pure . T.pack . take 500 $ cycle ('\n' : ['a'..'z'])


data Todo = Todo
  { _todo_label :: Text
  , _todo_done :: Bool
  }
  deriving (Show, Read, Eq, Ord)

checkbox
  :: (MonadHold t m, MonadFix m, Reflex t)
  => Bool
  -> VtyWidget t m (Dynamic t Bool)
checkbox v0 = do
  i <- input
  v <- toggle v0 $ fforMaybe i $ \case
    V.EvMouseUp _ _ _ -> Just ()
    _ -> Nothing
  text $ current $ ffor v $ \v' -> if v' then "[x]" else "[ ]"
  return v

button :: (Reflex t, Monad m) => Behavior t Text -> VtyWidget t m (Event t ())
button t = do
  box roundedBoxStyle $ text t
  fmap (() <$) mouseUp

todo
  :: (MonadHold t m, MonadFix m, Reflex t)
  => Todo
  -> VtyWidget t m (Dynamic t Todo)
todo t0 = do
  w <- displayWidth
  let checkboxWidth = 3
      checkboxRegion = pure $ Region 0 0 checkboxWidth 1
      labelRegion = ffor w $ \w' -> Region (checkboxWidth + 1) 0 (w' - 1 - checkboxWidth) 1
  value <- pane checkboxRegion (pure True) $ checkbox $ _todo_done t0
  label <- pane labelRegion (pure True) $
    textInput $ def { _textInputConfig_initialValue = TZ.fromText $ _todo_label t0 }
  return $ Todo <$> label <*> value

todos
  :: (MonadHold t m, MonadFix m, Reflex t, Adjustable t m, NotReady t m, PostBuild t m)
  => [Todo]
  -> Event t ()
  -> VtyWidget t m (Dynamic t (Seq Todo))
todos todos0 newTodo = do
  rec todos <- foldDyn ($) (Seq.fromList todos0) $ leftmost
        [ (\ts -> ts Seq.|> Todo "" False) <$ newTodo
        , (\(ix, t) -> Seq.update ix t) <$> updates
        ]
      w <- displayWidth
      listOut <- networkView $ ffor todos $ \ts' ->
        flip Seq.traverseWithIndex ts' $ \row t -> do
          let reg = fmap (\w' -> Region 0 row w' 1) w
          pane reg (fmap (==row) selected) $ do
            e <- mouseUp
            r <- todo t
            return (row <$ e, (row,) <$> updated r)
      selectionClick <- switchHold never $
        fmap (leftmost . toList . fmap fst) listOut
      selected <- holdDyn 0 $ leftmost
        [ selectionClick
        , Seq.length <$> tag (current todos) newTodo
        ]
      updates <- switchHold never $ fmap (leftmost . toList . fmap snd) listOut
  return todos

todos'
  :: (MonadHold t m, MonadFix m, Reflex t, Adjustable t m, NotReady t m, PostBuild t m)
  => [Todo]
  -> Event t ()
  -> VtyWidget t m (Dynamic t (Map Int Todo))
todos' todos0 newTodo = do
  let todosMap0 = Map.fromList $ zip [0..] todos0
  w <- displayWidth
  rec listOut <- listHoldWithKey todosMap0 insert $ \row t -> do
        let reg = fmap (\w' -> Region 0 row w' 1) w
        pane reg (fmap (==row) selected) $ do
          e <- mouseUp
          r <- todo t
          return (row <$ e, r)
      let selectionClick = switch . current $ fmap (leftmost . Map.elems . fmap fst) listOut
      selected <- holdDyn 0 $ leftmost
        [ selectionClick
        , Map.size <$> tag (current todosMap) newTodo
        ]
      let todosMap = joinDynThroughMap $ fmap (fmap snd) listOut
      let insert = ffor (tagPromptlyDyn todosMap newTodo) $ \m -> case Map.lookupMax m of
            Nothing -> Map.singleton 0 $ Just $ Todo "" False
            Just (k, _) -> {- Map.union (Just <$> m) $ -} Map.singleton (k+1) $ Just $ Todo "" False
  return todosMap
