import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network

import qualified Data.Text.Zipper as TZ
import Data.Text.Zipper (TextAlignment (..))
import Example.CPU
import Reflex.Vty

type VtyExample t m =
  ( MonadFix m
  , MonadHold t m
  , Reflex t
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocus t m
  , HasFocusReader t m
  , HasTheme t m
  , HasColorProfile t m
  )

type Manager t m =
  ( HasLayout t m
  , HasFocus t m
  )

data Example
  = Example_TextEditor
  | Example_Todo
  | Example_ScrollableTextDisplay
  | Example_ClickButtonsGetEmojis
  | Example_CPUStat
  | Example_Scrollable
  | Example_Showcase
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

withCtrlC :: (Monad m, HasInput t m, Reflex t) => m () -> m (Event t ())
withCtrlC f = do
  inp <- input
  f
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

main :: IO ()
main = mainWidget $ withCtrlC $ do
  initManager_ $ do
    tabNavigation
    let gf = grout . fixed
        t = tile (fixed 3)
        buttons = col $ do
          gf 3 $ col $ do
            gf 1 $ text "Select an example."
            gf 1 $ text "Esc will bring you back here."
            gf 1 $ text "Ctrl+c to quit."
          a <- t $ textButtonStatic def "Todo List"
          b <- t $ textButtonStatic def "Text Editor"
          c <- t $ textButtonStatic def "Scrollable text display"
          d <- t $ textButtonStatic def "Clickable buttons"
          e <- t $ textButtonStatic def "CPU Usage"
          f <- t $ textButtonStatic def "Scrollable"
          g <- t $ textButtonStatic def "Showcase"
          return $
            leftmost
              [ Left Example_Todo <$ a
              , Left Example_TextEditor <$ b
              , Left Example_ScrollableTextDisplay <$ c
              , Left Example_ClickButtonsGetEmojis <$ d
              , Left Example_CPUStat <$ e
              , Left Example_Scrollable <$ f
              , Left Example_Showcase <$ g
              ]
    let escapable w = do
          void w
          i <- input
          return $ fforMaybe i $ \case
            V.EvKey V.KEsc [] -> Just $ Right ()
            _ -> Nothing
    rec out <- networkHold buttons $ ffor (switch (current out)) $ \case
          Left Example_Todo -> escapable taskList
          Left Example_TextEditor -> escapable $ localTheme (const (constant darkTheme)) testBoxes
          Left Example_ScrollableTextDisplay -> escapable scrolling
          Left Example_ClickButtonsGetEmojis -> escapable easyExample
          Left Example_CPUStat -> escapable cpuStats
          Left Example_Scrollable -> escapable scrollingWithLayout
          Left Example_Showcase -> escapable showcaseDemo
          Right () -> buttons
    return ()

scrollingWithLayout
  :: forall t m
   . ( VtyExample t m
     , HasInput t m
     , MonadHold t m
     , Manager t m
     , PostBuild t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     )
  => m ()
scrollingWithLayout = col $ do
  (s, _) <- tile flex $ boxTitle (constant TextAlignment_Center) (constant def) (constant "Tracks") $ scrollable def $ do
    result <- do
      forM_ [(0 :: Int) .. 10] $ \n -> do
        tile (fixed 5) $ do
          tile (fixed 4) $ textButtonStatic def $ T.pack (show n)
      askRegion
    pure (never, result)
  grout (fixed 1) $
    text $
      ("Total Lines: " <>) . T.pack . show <$> _scrollable_totalLines s
  grout (fixed 1) $
    text $
      ("Scroll Pos: " <>) . T.pack . show <$> _scrollable_scrollPosition s
  grout (fixed 1) $
    text $
      ("Scroll Height: " <>) . T.pack . show <$> _scrollable_scrollHeight s
  pure ()

-- * Mouse button and emojis example
easyExample :: (VtyExample t m, Manager t m, MonadHold t m) => m (Event t ())
easyExample = do
  row $ grout (fixed 39) $ col $ do
    (a1, b1, c1) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "POTATO"
      b <- tile flex $ btn "TOMATO"
      c <- tile flex $ btn "EGGPLANT"
      return (a, b, c)
    (a2, b2, c2) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "CHEESE"
      b <- tile flex $ btn "BEES"
      c <- tile flex $ btn "MY KNEES"
      return (a, b, c)
    (a3, b3, c3) <- grout (fixed 3) $ row $ do
      a <- tile flex $ btn "TIME"
      b <- tile flex $ btn "RHYME"
      c <- tile flex $ btn "A BIG CRIME"
      return (a, b, c)
    tile (fixed 7) $ boxTitle (constant TextAlignment_Center) (constant def) "CLICK BUTTONS TO DRAW*" $ do
      outputDyn <-
        foldDyn (<>) "" $
          mergeWith
            (<>)
            [a1 $> "\129364", b1 $> "🍅", c1 $> "🍆", a2 $> "\129472", b2 $> "🐝🐝", c2 $> "💘", a3 $> "⏰", b3 $> "📜", c3 $> "💰🔪🔒"]
      text (current outputDyn)
    tile flex $ text "* Requires font support for emojis. Box may render incorrectly unless your vty is initialized with an updated char width map."
  inp <- input
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
  where
    btn label = do
      let cfg = def {_buttonConfig_focusStyle = pure doubleBoxStyle}
      buttonClick <- textButtonStatic cfg label
      keyPress <-
        keyCombos $
          Set.fromList
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
  deriving (Eq, Ord, Read, Show)

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
  let toggleKeys =
        Set.fromList
          [ (V.KChar ' ', [V.MCtrl])
          , (V.KChar '@', [V.MCtrl])
          ]
  anyChildFocused $ \focused -> do
    toggleE <- keyCombos toggleKeys
    filterKeys (flip Set.notMember $ Set.insert (V.KChar '\t', []) toggleKeys) $ do
      rec let cfg =
                def
                  { _checkboxConfig_setValue = setVal
                  }
          value <- tile (fixed 4) $ checkbox cfg $ _todo_done t0
          let setVal = attachWith (\v _ -> not v) (current value) $ gate (current focused) toggleE
          (fid, (ti, d)) <- tile' flex $ do
            i <- input
            v <-
              textInput $
                def
                  { _textInputConfig_initialValue = TZ.fromText $ _todo_label t0
                  }
            let deleteSelf = attachWithMaybe backspaceOnEmpty (current $ _textInput_value v) i
            return (v, deleteSelf)
      return $
        TodoOutput
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
  :: forall t m
   . ( MonadHold t m
     , Manager t m
     , VtyExample t m
     , Adjustable t m
     , PostBuild t m
     )
  => [Todo]
  -> Event t ()
  -> m (Dynamic t (Map Int (TodoOutput t)))
todos todos0 newTodo = do
  let todosMap0 = Map.fromList $ zip [0 ..] todos0
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
            Just (k, _) -> Map.singleton (k + 1) $ Just $ Todo "" False
          updates = leftmost [insert, delete]
          todoDelete =
            switch . current $
              leftmost . Map.elems . Map.mapWithKey (\k -> (k <$) . _todoOutput_delete) <$> listOut

          selectOnDelete =
            fanMap $
              (`Map.singleton` ())
                <$> attachWithMaybe
                  ( \m k ->
                      let (before, after) = Map.split k m
                      in fmap fst $ Map.lookupMax before <|> Map.lookupMin after
                  )
                  (current todosMap)
                  todoDelete
  return listOut

-- * Scrollable text example

scrolling
  :: ( VtyExample t m
     , MonadHold t m
     , Manager t m
     , PostBuild t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     )
  => m ()
scrolling = col $ do
  grout (fixed 2) $ text "Use your mouse wheel or up and down arrows to scroll:"
  (fid, out) <- tile' (fixed 5) $ boxStatic def $ scrollableText def $ "Gallia est omnis divisa in partes tres, quarum unam incolunt Belgae, aliam Aquitani, tertiam qui ipsorum lingua Celtae, nostra Galli appellantur. Hi omnes lingua, institutis, legibus inter se differunt. Gallos ab Aquitanis Garumna flumen, a Belgis Matrona et Sequana dividit. Horum omnium fortissimi sunt Belgae, propterea quod a cultu atque humanitate provinciae longissime absunt, minimeque ad eos mercatores saepe commeant atque ea quae ad effeminandos animos pertinent important, proximique sunt Germanis, qui trans Rhenum incolunt, quibuscum continenter bellum gerunt. Qua de causa Helvetii quoque reliquos Gallos virtute praecedunt, quod fere cotidianis proeliis cum Germanis contendunt, cum aut suis finibus eos prohibent aut ipsi in eorum finibus bellum gerunt. Eorum una pars, quam Gallos obtinere dictum est, initium capit a flumine Rhodano, continetur Garumna flumine, Oceano, finibus Belgarum, attingit etiam ab Sequanis et Helvetiis flumen Rhenum, vergit ad septentriones. Belgae ab extremis Galliae finibus oriuntur, pertinent ad inferiorem partem fluminis Rheni, spectant in septentrionem et orientem solem. Aquitania a Garumna flumine ad Pyrenaeos montes et eam partem Oceani quae est ad Hispaniam pertinet; spectat inter occasum solis et septentriones.\nApud Helvetios longe nobilissimus fuit et ditissimus Orgetorix. Is M. Messala, [et P.] M. Pisone consulibus regni cupiditate inductus coniurationem nobilitatis fecit et civitati persuasit ut de finibus suis cum omnibus copiis exirent: perfacile esse, cum virtute omnibus praestarent, totius Galliae imperio potiri. Id hoc facilius iis persuasit, quod undique loci natura Helvetii continentur: una ex parte flumine Rheno latissimo atque altissimo, qui agrum Helvetium a Germanis dividit; altera ex parte monte Iura altissimo, qui est inter Sequanos et Helvetios; tertia lacu Lemanno et flumine Rhodano, qui provinciam nostram ab Helvetiis dividit. His rebus fiebat ut et minus late vagarentur et minus facile finitimis bellum inferre possent; qua ex parte homines bellandi cupidi magno dolore adficiebantur. Pro multitudine autem hominum et pro gloria belli atque fortitudinis angustos se fines habere arbitrabantur, qui in longitudinem milia passuum CCXL, in latitudinem CLXXX patebant."
  pb <- getPostBuild
  requestFocus $ Refocus_Id fid <$ pb
  grout (fixed 1) $ text $ ffor (_scrollable_scrollPosition out) $ \p ->
    "Scrolled to " <> case p of
      ScrollPos_Top -> "top"
      ScrollPos_Bottom -> "bottom"
      ScrollPos_Line n -> "line " <> T.pack (show n)
  e <- performEventAsync $ ffor pb $ \_ cb -> liftIO $ void $ forkIO $ forever $ do
    threadDelay 1000000
    t <- getCurrentTime
    cb $ [T.pack $ show t]
  xs <- foldDyn (flip (<>)) [] e
  grout (fixed 3) blank
  tile (fixed 10) $ col $ do
    grout (fixed 1) $ text "This one scrolls automatically as the output grows:"
    Scrollable pos total h <-
      tile flex $
        scrollableText (ScrollableConfig never never ScrollPos_Bottom (pure $ Just ScrollToBottom_Maintain)) $
          T.unlines <$> xs
    grout (fixed 5) $ boxStatic def $ do
      grout (fixed 1) $ row $ do
        grout (fixed 8) (text "Height:")
        grout flex $ display h
      grout (fixed 1) $ row $ do
        grout (fixed 8) $ text "Scroll:"
        grout flex $ display pos
      grout (fixed 1) $ row $ do
        grout (fixed 8) $ text "Length:"
        grout flex $ display total

--  * Text editor example with resizable boxes

testBoxes
  :: (MonadHold t m, VtyExample t m)
  => m ()
testBoxes = do
  dw <- displayWidth
  dh <- displayHeight
  let region1 = Region <$> (div' dw 6) <*> (div' dh 6) <*> (div' dw 2) <*> (div' dh 2)
      region2 = Region <$> (div' dw 4) <*> (div' dh 4) <*> (2 * div' dw 3) <*> (2 * div' dh 3)
  pane region1 (constDyn False) . boxStatic singleBoxStyle $ debugInput
  _ <-
    pane region2 (constDyn True) . boxStatic singleBoxStyle $
      let cfg =
            def
              { _textInputConfig_initialValue =
                  "This box is a text input. The box below responds to mouse drag inputs. You can also drag the separator between the boxes to resize them."
              }
          textBox =
            boxTitle (pure TextAlignment_Center) (pure roundedBoxStyle) "Text Edit" $
              multilineTextInput cfg
          dragBox = boxStatic roundedBoxStyle dragTest
      in splitVDrag (hRule doubleBoxStyle) textBox dragBox
  return ()
  where
    div' :: (Integral a, Applicative f) => f a -> f a -> f a
    div' = liftA2 div

debugInput :: (VtyExample t m, MonadHold t m) => m ()
debugInput = do
  lastEvent <- hold "No event yet" . fmap show =<< input
  text $ T.pack <$> lastEvent

dragTest :: (VtyExample t m, MonadHold t m) => m ()
dragTest = do
  lastEvent <- hold "No event yet" . fmap show =<< drag V.BLeft
  text $ T.pack <$> lastEvent

-- * Showcase: one screen showing off all styling, theming, and color

-- profiling. Tab cycles the predefined themes; the whole screen is
-- rendered under the current theme.
showcaseDemo :: (VtyExample t m, MonadHold t m, HasLayout t m, HasColorProfile t m) => m ()
showcaseDemo = do
  tab <- key (V.KChar '\t')
  nDyn <- foldDyn (\_ n -> n + 1) 0 tab
  prof <- colorProfile
  let themes =
        cycle
          [ ("default", defTheme)
          , ("dark", darkTheme)
          , ("charm", charmTheme)
          , ("dracula", draculaTheme)
          , ("nord", nordTheme)
          , ("zenburn", zenburnTheme)
          , ("gruvbox", gruvboxTheme)
          ]
      pick n = drop (n `mod` 7) themes
      curTheme n = case pick n of
        (_, th) : _ -> th
        [] -> defTheme
      curLabel n = case pick n of
        (label, _) : _ -> label
        [] -> ""
      themeBeh = curTheme <$> current nDyn
      headerBeh =
        (\n p -> T.pack ("Theme: " <> curLabel n <> "  |  Profile: " <> show p <> "  |  Tab cycles  |  Esc back"))
          <$> current nDyn
          <*> prof
  localTheme (const themeBeh) $ do
    fill (pure ' ')
    col $ do
      grout (fixed 1) $ text headerBeh
      grout flex $ row $ do
        -- Left column: style samples
        grout flex $ col $ do
          grout (fixed 1) $ text "Borders:"
          grout (fixed 3) $ row $ do
            grout flex $ styledImage "single" (withBorder singleBorder def)
            grout flex $ styledImage "rounded" (withBorder roundedBorder def)
            grout flex $ styledImage "thick" (withBorder thickBorder def)
            grout flex $ styledImage "double" (withBorder doubleBorder def)
            grout flex $ styledImage "ascii" (withBorder asciiBorder def)
          grout (fixed 1) $ text "Padding/Margin:"
          grout (fixed 3) $ row $ do
            grout flex $ styledImage "pad 1" (withPadding 1 1 1 1 def)
            grout flex $ styledImage "pad 2" (withPadding 2 2 2 2 def)
            grout flex $ styledImage "margin 1" (withMargin 1 1 1 1 def)
          grout (fixed 1) $ text "Colors:"
          grout (fixed 3) $ row $ do
            grout flex $ styledImage "red fg" (withForeground red def)
            grout flex $ styledImage "blue bg" (withBackground blue def)
            grout flex $ styledImage "rgb" (withForeground (rgbColor 200 100 50) def)
          grout (fixed 1) $ text "Transforms:"
          grout (fixed 3) $ row $ do
            grout flex $ styledImage "bold" (withBold def)
            grout flex $ styledImage "italic" (withItalic def)
            grout flex $ styledImage "underline" (withUnderline UnderlineSingle def)
            grout flex $ styledImage "reverse" (withReverse def)
          grout (fixed 1) $ text "Alignment:"
          grout (fixed 3) $ row $ do
            grout flex $ styledImage "left" (withAlignH HAlignLeft . withWidth 20 $ def)
            grout flex $ styledImage "center" (withAlignH HAlignCenter . withWidth 20 $ def)
            grout flex $ styledImage "right" (withAlignH HAlignRight . withWidth 20 $ def)
          grout (fixed 1) $ text "Combined & Hyperlink:"
          grout (fixed 5) $ row $ do
            grout flex $
              styledImage
                "combined"
                ( withBorder roundedBorder
                    . withPadding 1 2 1 2
                    . withForeground brightGreen
                    . withBorderForeground brightMagenta
                    $ def
                )
            grout flex $
              styledImage
                "link"
                (withHyperlink "https://reflex-frp.org" . withUnderline UnderlineSingle $ def)
        -- Right column: themed widgets + color profile swatches
        grout flex $ col $ do
          grout (fixed 1) $ text "Themed Widgets:"
          void $ grout (fixed 3) $ textButtonStatic def "A button"
          void $ grout (fixed 3) $ checkbox def False
          void $ grout (fixed 3) $ linkStatic "A link"
          void $ grout (fixed 3) $ textInput def
          grout (fixed 1) $ text "Color Profile Swatches:"
          grout (fixed 2) $ row $ do
            grout flex $ profileSwatch "TrueColor" ColorProfile_TrueColor
            grout flex $ profileSwatch "Ansi256" ColorProfile_Ansi256
            grout flex $ profileSwatch "Ansi16" ColorProfile_Ansi16
            grout flex $ profileSwatch "Ascii" ColorProfile_Ascii
            grout flex $ profileSwatch "NoTTY" ColorProfile_NoTTY
  where
    orange = rgbColor 200 100 50
    styledImage label s = do
      th <- theme
      tellImages $ (\t -> [render (inherit (_theme_default t) s) label]) <$> th
    profileSwatch label prof = do
      bt <- themeAttr
      tellImages $ (\a -> [V.text' (applyProfile prof (V.withForeColor a orange)) (label <> " ")]) <$> bt
