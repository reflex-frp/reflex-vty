{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.NodeId
import Data.Functor
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Vty

type VtyExample t m =
  ( MonadFix m
  , Reflex t
  , HasVtyInput t m
  , HasVtyWidgetCtx t m
  , ImageWriter t m
  , MonadNodeId m
  , HasDisplaySize t m
  , HasFocus t m
  )

type Manager t m =
  ( MonadLayout t m
  , MonadFocus t m
  )

data Example = Example_TextEditor
             | Example_Todo
             | Example_ScrollableTextDisplay
             | Example_ClickButtonsGetEmojis
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

main :: IO ()
main = mainWidget $ do
  inp <- input
  initManager_ $ do
    tabNavigation
    let gf = grout . fixed
        tf = tile . fixed
        buttons = col $ do
          gf 4 $ col $ do
            gf 1 $ text "Select an example."
            gf 1 $ text "Esc will bring you back here."
            gf 1 $ text "Ctrl+c to quit."
          a <- tf 5 $ textButtonStatic def "Todo List"
          b <- tf 5 $ textButtonStatic def "Text Editor"
          c <- tf 5 $ textButtonStatic def "Scrollable text display"
          d <- tf 5 $ textButtonStatic def "Clickable buttons"
          return $ leftmost
            [ Left Example_Todo <$ a
            , Left Example_TextEditor <$ b
            , Left Example_ScrollableTextDisplay <$ c
            , Left Example_ClickButtonsGetEmojis <$ d
            ]
    let escapable w = do
          void w
          i <- input
          return $ fforMaybe i $ \case
            V.EvKey V.KEsc [] -> Just $ Right ()
            _ -> Nothing
    rec out <- networkHold buttons $ ffor (switch (current out)) $ \case
          Left Example_Todo -> escapable taskList
          Left Example_TextEditor -> escapable testBoxes
          Left Example_ScrollableTextDisplay -> escapable scrolling
          Left Example_ClickButtonsGetEmojis -> escapable easyExample
          Right () -> buttons
    return ()
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

-- * Mouse button and emojis example
easyExample :: (VtyExample t m, Manager t m, MonadHold t m) => m (Event t ())
easyExample = do
  row $ grout (fixed 39) $ col $ do
    (a1,b1,c1) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "POTATO"
      b <- tile flex $ btn "TOMATO"
      c <- tile flex $ btn "EGGPLANT"
      return (a,b,c)
    (a2,b2,c2) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "CHEESE"
      b <- tile flex $ btn "BEES"
      c <- tile flex $ btn "MY KNEES"
      return (a,b,c)
    (a3,b3,c3) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "TIME"
      b <- tile flex $ btn "RHYME"
      c <- tile flex $ btn "A BIG CRIME"
      return (a,b,c)
    tile (fixed 7) $ boxTitle (constant def) "CLICK BUTTONS TO DRAW*" $ do
      outputDyn <- foldDyn (<>) "" $ mergeWith (<>)
        [a1 $> "\129364", b1 $> "🍅", c1 $> "🍆", a2 $> "\129472", b2 $> "🐝🐝", c2 $> "💘", a3 $> "⏰", b3 $> "📜", c3 $> "💰🔪🔒"]
      text (current outputDyn)
    tile flex $ text "* Requires font support for emojis. Box may render incorrectly unless your vty is initialized with an updated char width map."
  inp <- input
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
  where
    btn label = do
      let cfg = def { _buttonConfig_focusStyle = pure doubleBoxStyle }
      buttonClick <- textButtonStatic cfg label
      keyPress <- keyCombos $ Set.fromList
        [ (V.KEnter, [])
        , (V.KChar ' ', [])
        ]
      pure $ leftmost [() <$ buttonClick, () <$ keyPress]

-- * Task list example
taskList
  :: (VtyExample t m, Manager t m, MonadHold t m, Adjustable t m, PostBuild t m)
  => m ()
taskList = col $ do
  let todos0 =
        [ Todo "Find reflex-vty" True
        , Todo "Become functional reactive" False
        , Todo "Make vty apps" False
        ]
      btn = textButtonStatic def "Add another task"
  enter <- fmap (const ()) <$> key V.KEnter
  rec void $ grout flex $ todos todos0 $ enter <> click
      click <- tile (fixed 3) btn
  return ()

data Todo = Todo
  { _todo_label :: Text
  , _todo_done :: Bool
  }
  deriving (Show, Read, Eq, Ord)

data TodoOutput t = TodoOutput
  { _todoOutput_todo :: Dynamic t Todo
  , _todoOutput_delete :: Event t ()
  , _todoOutput_height :: Dynamic t Int
  , _todoOutput_focusId :: FocusId
  }

todo
  :: (VtyExample t m, Manager t m, MonadHold t m)
  => Todo
  -> m (TodoOutput t)
todo t0 = row $ do
  let getToggleEvent = keyCombos $ Set.fromList
        [ (V.KChar ' ', [V.MCtrl])
        , (V.KChar '@', [V.MCtrl])
        ]
  rec let cfg = def
            { _checkboxConfig_setValue = setVal
            }
      (toggleCB, value) <- tile (fixed 4) $ do
        e <- getToggleEvent
        v <- checkbox cfg $ _todo_done t0
        pure (e, v)
      let setVal = attachWith (\v _ -> not v) (current value) $ leftmost
            [ toggleCB
            , toggleTI
            ]
      (fid, (ti, toggleTI, d)) <- tile' flex $ do
        i <- input
        e <- getToggleEvent
        v <- textInput $ def
          { _textInputConfig_initialValue = TZ.fromText $ _todo_label t0 }
        let deleteSelf = attachWithMaybe backspaceOnEmpty (current $ _textInput_value v) i
        return (v, e, deleteSelf)
  return $ TodoOutput
    { _todoOutput_todo = Todo <$> _textInput_value ti <*> value
    , _todoOutput_delete = d
    , _todoOutput_height = _textInput_lines ti
    , _todoOutput_focusId = fid
    }
  where
    backspaceOnEmpty v = \case
      V.EvKey V.KBS _ | T.null v -> Just ()
      _ -> Nothing

todos
  :: forall t m.
     ( MonadHold t m
     , Manager t m
     , VtyExample t m
     , Adjustable t m
     , PostBuild t m
     )
  => [Todo]
  -> Event t ()
  -> m (Dynamic t (Map Int (TodoOutput t)))
todos todos0 newTodo = do
  let todosMap0 = Map.fromList $ zip [0..] todos0
  rec listOut <- listHoldWithKey todosMap0 updates $ \k t -> grout (fixed 1) $ do
        to <- todo t
        let sel = select selectOnDelete $ Const2 k
        pb <- getPostBuild
        requestFocus $ Refocus_Id (_todoOutput_focusId to) <$ leftmost [pb, sel]
        pure to
      let delete = flip Map.singleton Nothing <$> todoDelete
          todosMap = joinDynThroughMap $ fmap _todoOutput_todo <$> listOut
          insert = ffor (tag (current todosMap) newTodo) $ \m -> case Map.lookupMax m of
             Nothing -> Map.singleton 0 $ Just $ Todo "" False
             Just (k, _) -> Map.singleton (k+1) $ Just $ Todo "" False
          updates = leftmost [insert, delete]
          todoDelete = switch . current $
            leftmost .  Map.elems . Map.mapWithKey (\k -> (k <$) . _todoOutput_delete) <$> listOut

          selectOnDelete = fanMap $ (`Map.singleton` ()) <$> attachWithMaybe
            (\m k -> let (before, after) = Map.split k m
                      in  fmap fst $ Map.lookupMax before <|> Map.lookupMin after)
            (current todosMap)
            todoDelete
  return listOut

-- * Scrollable text example

scrolling :: (VtyExample t m, MonadHold t m, Manager t m, PostBuild t m) => m ()
scrolling = col $ do
  grout (fixed 2) $ text "Use your mouse wheel or up and down arrows to scroll:"
  (fid, out) <- tile' (fixed 5) $ boxStatic def $ scrollableText never $ "Gallia est omnis divisa in partes tres, quarum unam incolunt Belgae, aliam Aquitani, tertiam qui ipsorum lingua Celtae, nostra Galli appellantur. Hi omnes lingua, institutis, legibus inter se differunt. Gallos ab Aquitanis Garumna flumen, a Belgis Matrona et Sequana dividit. Horum omnium fortissimi sunt Belgae, propterea quod a cultu atque humanitate provinciae longissime absunt, minimeque ad eos mercatores saepe commeant atque ea quae ad effeminandos animos pertinent important, proximique sunt Germanis, qui trans Rhenum incolunt, quibuscum continenter bellum gerunt. Qua de causa Helvetii quoque reliquos Gallos virtute praecedunt, quod fere cotidianis proeliis cum Germanis contendunt, cum aut suis finibus eos prohibent aut ipsi in eorum finibus bellum gerunt. Eorum una pars, quam Gallos obtinere dictum est, initium capit a flumine Rhodano, continetur Garumna flumine, Oceano, finibus Belgarum, attingit etiam ab Sequanis et Helvetiis flumen Rhenum, vergit ad septentriones. Belgae ab extremis Galliae finibus oriuntur, pertinent ad inferiorem partem fluminis Rheni, spectant in septentrionem et orientem solem. Aquitania a Garumna flumine ad Pyrenaeos montes et eam partem Oceani quae est ad Hispaniam pertinet; spectat inter occasum solis et septentriones.\nApud Helvetios longe nobilissimus fuit et ditissimus Orgetorix. Is M. Messala, [et P.] M. Pisone consulibus regni cupiditate inductus coniurationem nobilitatis fecit et civitati persuasit ut de finibus suis cum omnibus copiis exirent: perfacile esse, cum virtute omnibus praestarent, totius Galliae imperio potiri. Id hoc facilius iis persuasit, quod undique loci natura Helvetii continentur: una ex parte flumine Rheno latissimo atque altissimo, qui agrum Helvetium a Germanis dividit; altera ex parte monte Iura altissimo, qui est inter Sequanos et Helvetios; tertia lacu Lemanno et flumine Rhodano, qui provinciam nostram ab Helvetiis dividit. His rebus fiebat ut et minus late vagarentur et minus facile finitimis bellum inferre possent; qua ex parte homines bellandi cupidi magno dolore adficiebantur. Pro multitudine autem hominum et pro gloria belli atque fortitudinis angustos se fines habere arbitrabantur, qui in longitudinem milia passuum CCXL, in latitudinem CLXXX patebant."
  pb <- getPostBuild
  requestFocus $ Refocus_Id fid <$ pb
  grout (fixed 1) $ text $ ffor out $ \(ix, total) -> "Scrolled to line " <> T.pack (show ix) <> " of " <> T.pack (show total)

--  * Text editor example with resizable boxes

testBoxes
  :: (MonadHold t m, MonadNodeId m, VtyExample t m)
  => m ()
testBoxes = do
  dw <- displayWidth
  dh <- displayHeight
  let region1 = Region <$> (div' dw 6) <*> (div' dh 6) <*> (div' dw 2) <*> (div' dh 2)
      region2 = Region <$> (div' dw 4) <*> (div' dh 4) <*> (2 * div' dw 3) <*> (2 * div' dh 3)
  pane region1 (constDyn False) . boxStatic singleBoxStyle $ debugInput
  _ <- pane region2 (constDyn True) . boxStatic singleBoxStyle $
    let cfg = def
          { _textInputConfig_initialValue =
            "This box is a text input. The box below responds to mouse drag inputs. You can also drag the separator between the boxes to resize them."
          }
        textBox = boxTitle (pure roundedBoxStyle) "Text Edit" $
          multilineTextInput cfg
        dragBox = boxStatic roundedBoxStyle dragTest
    in splitVDrag (hRule doubleBoxStyle) textBox dragBox
  return ()
  where
    div' :: (Integral a, Applicative f) => f a -> f a -> f a
    div' = liftA2 div

debugFocus :: (VtyExample t m) => m ()
debugFocus = do
  f <- focus
  text $ T.pack . show <$> current f

debugInput :: (VtyExample t m, MonadHold t m) => m ()
debugInput = do
  lastEvent <- hold "No event yet" . fmap show =<< input
  text $ T.pack <$> lastEvent

dragTest :: (VtyExample t m, MonadHold t m) => m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  text $ T.pack <$> lastEvent

testStringBox :: VtyExample t m => m ()
testStringBox = boxStatic singleBoxStyle .
  text . pure . T.pack . take 500 $ cycle ('\n' : ['a'..'z'])
