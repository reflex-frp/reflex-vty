{-# LANGUAGE PackageImports #-}

-- | A tiny newsreader: a keyed article list and a full-screen reading view.
--
-- Up and Down move the selection, Enter reads the selected article and
-- Escape returns to the list. Reading an article marks it read, so its
-- bullet becomes @○@; unread articles keep @●@. @q@ quits from the list
-- only, and Ctrl+C quits from either view. Selection and read state live
-- in the process and survive every round trip.
module Example.Articles (runArticles, articlesWidget) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Ordered as OMap
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper (textWidth)
import qualified Graphics.Vty as V
import Reflex
import "reflex-vty" Reflex.Vty

import "reflex-vty-widgets" Reflex.Vty.Widget

-- | The list renders in key order, so this order is the screen order.
data ArticleKey
  = ArticleRelease
  | ArticleScrolling
  | ArticleGrid
  deriving (Eq, Ord, Show)

articles :: [(ArticleKey, Text)]
articles =
  [ (ArticleRelease, "Reflex 1.3 released")
  , (ArticleScrolling, "Terminal scrolling deep dive")
  , (ArticleGrid, "Grid layout tips")
  ]

articleTitle :: ArticleKey -> Text
articleTitle k = fromMaybe "" (lookup k articles)

articleBody :: ArticleKey -> [Text]
articleBody = \case
  ArticleRelease ->
    [ "Reflex 1.3 is out."
    , "Smaller builds, faster startup,"
    , "and clearer errors throughout."
    , "Read the ChangeLog before upgrading."
    ]
  ArticleScrolling ->
    [ "Scrolling is more than moving text."
    , "A good viewport keeps the selection,"
    , "the cursor, and the scrollbar"
    , "in agreement on every frame."
    ]
  ArticleGrid ->
    [ "Give every panel a fixed track."
    , "Let one flexible region absorb the slack."
    , "Prefer boring constraints:"
    , "they survive resizing and rotation."
    ]

-- | Widest rendered row, so every row shares one centred block.
articleLineWidth :: Int
articleLineWidth = maximum (map (textWidth . ("● " <>) . snd) articles)

listHint :: Text
listHint = "Up/Down: Select | Enter: Read | q: Quit"

runArticles :: IO ()
runArticles = mainWidget def articlesWidget

articlesWidget
  :: ( MonadVtyApp t m
     , MonadNodeId m
     , HasImageWriter t m
     , HasDisplayRegion t m
     , HasFocusReader t m
     , HasInput t m
     , HasTheme t m
     , HasColorProfile t m
     , HasCursor t m
     , HasScreenMode t m
     )
  => m (Event t ())
articlesWidget = do
  enterAlternateScreen
  events <- input
  vp <- askViewport
  let ctrlC = fforMaybe events $ \case
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
      overlayConfig =
        def
          { _overlayConfig_size =
              (\r -> (_region_width r, _region_height r)) <$> vp
          , _overlayConfig_position = pure (OverlayAt 0 0)
          , _overlayConfig_backdrop = pure (SolidBackdrop def)
          , _overlayConfig_panelStyle = pure Nothing
          }
  rec requested <-
        holdDyn False $
          leftmost
            [ True <$ activateE
            , False <$ _overlay_dismissed outer
            ]
      readD <-
        foldDyn
          (\k readKeys -> Set.insert k readKeys)
          Set.empty
          activateE
      outer <-
        modal
          overlayConfig
          requested
          (listView readD)
          (readingView selectionD)
      let (activateE, selectionD, quitE) = _overlay_background outer
  pure $ leftmost [ctrlC, quitE]

-- | The list screen: heading, ordered keyed rows, and the hint. The list
-- owns @q@ only; Up, Down, and Enter reach 'selectList'.
listView
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     , Adjustable t m
     , MonadNodeId m
     , PostBuild t m
     , HasInput t m
     , HasFocus t m
     , HasFocusReader t m
     , HasDisplayRegion t m
     , HasImageWriter t m
     , HasTheme t m
     , HasLayout t m
     , HasCursor t m
     )
  => Dynamic t (Set ArticleKey)
  -> m (Event t ArticleKey, Dynamic t (Maybe ArticleKey), Event t ())
listView readD = do
  (actions, sl) <-
    withKeyMap (pure listKeyMap) $
      col $ do
        grout flex blank
        grout (fixed (pure 1)) $ centeredLine (pure "Articles")
        selected <-
          grout (fixed (pure (length articles))) $
            selectList
              def
              (pure (OMap.fromList articles))
              (articleRow readD)
        grout (fixed (pure 1)) $ centeredLine (pure listHint)
        grout flex blank
        pure selected
  pure
    ( _selectList_activate sl
    , _selectList_selection sl
    , command ListQuit actions
    )

-- | The reading view: a full-screen modal over the list. Escape is the
-- overlay's dismissal key; @q@ is ordinary (ignored) input here.
readingView
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     , MonadNodeId m
     , PostBuild t m
     , HasInput t m
     , HasFocusReader t m
     , HasDisplayRegion t m
     , HasImageWriter t m
     , HasTheme t m
     , HasLayout t m
     , HasCursor t m
     )
  => Dynamic t (Maybe ArticleKey)
  -> m ()
readingView selectionD = do
  let titleD = maybe "" articleTitle <$> selectionD
      bodyD =
        maybe "" (T.intercalate "\n" . articleBody)
          <$> selectionD
  col $ do
    grout flex blank
    grout (fixed (pure 1)) $ centeredLine (current titleD)
    grout (fixed (pure 1)) blank
    grout (fixed (pure 4)) $ centeredBlock (current bodyD)
    grout (fixed (pure 1)) blank
    grout (fixed (pure 1)) $ centeredLine (pure "Escape: Back")
    grout flex blank

-- | One list row. The renderer fills the height-1 region that
-- 'selectList' allocates; the leading indent is unstyled, while the
-- selected row's bullet and title are drawn reverse-video.
articleRow
  :: ( Reflex t
     , Monad m
     , HasDisplayRegion t m
     , HasImageWriter t m
     , HasTheme t m
     )
  => Dynamic t (Set ArticleKey)
  -> ArticleKey
  -> Dynamic t Text
  -> Dynamic t Bool
  -> m ()
articleRow readD articleKey titleD selectedD = do
  width <- displayWidth
  attrs <- themeAttr
  let bulletD =
        (\readKeys -> if Set.member articleKey readKeys then "○ " else "● ")
          <$> readD
      indentD =
        (\w -> T.replicate (max 0 ((w - articleLineWidth) `div` 2)) " ")
          <$> width
      lineD = (<>) <$> bulletD <*> titleD
  tellImages $
    ( \attr indent line selected ->
        [ V.horizCat
            [ V.text' attr indent
            , V.text'
                (if selected then V.withStyle attr V.reverseVideo else attr)
                line
            ]
        ]
    )
      <$> attrs
      <*> current indentD
      <*> current lineD
      <*> current selectedD

-- | Center a single line by padding its left edge with the floor of the
-- horizontal slack.
centeredLine
  :: ( Reflex t
     , Monad m
     , HasDisplayRegion t m
     , HasImageWriter t m
     , HasTheme t m
     )
  => Behavior t Text
  -> m ()
centeredLine value = do
  width <- displayWidth
  text $
    ( \w s ->
        T.replicate (max 0 ((w - textWidth s) `div` 2)) " " <> s
    )
      <$> current width
      <*> value

-- | Center every logical line of a block independently.
centeredBlock
  :: ( Reflex t
     , Monad m
     , HasDisplayRegion t m
     , HasImageWriter t m
     , HasTheme t m
     )
  => Behavior t Text
  -> m ()
centeredBlock value = do
  width <- displayWidth
  text $
    ( \w s ->
        T.intercalate
          "\n"
          [ T.replicate (max 0 ((w - textWidth line) `div` 2)) " " <> line
          | line <- T.splitOn "\n" s
          ]
    )
      <$> current width
      <*> value

-- | The list screen owns @q@; the reading view does not.
data ListCmd = ListQuit
  deriving (Eq, Show)

listKeyMap :: KeyMap ListCmd
listKeyMap =
  KeyMap
    [ KeyBinding ListQuit [(V.KChar 'q', [])] "quit" True
    ]

command :: Reflex t => ListCmd -> Event t ListCmd -> Event t ()
command cmd = void . ffilter (== cmd)
