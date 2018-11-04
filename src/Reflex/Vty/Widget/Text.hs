{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Vty.Widget.Text where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State

import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V

-- | A zipper of the logical text input contents. The lines before the line containing the cursor
-- are stored in reverse order. The cursor is logically _between_ the "before" and "after" text.
data TextZipper = TextZipper
  { _textZipper_linesBefore :: [Text] -- reversed
  , _textZipper_before :: Text
  , _textZipper_after :: Text -- The cursor is on top of the first character of this text
  , _textZipper_linesAfter :: [Text]
  }
  deriving (Show)

left :: TextZipper -> TextZipper
left z@(TextZipper lb b a la) = case T.unsnoc b of
  Nothing -> case lb of
    [] -> z
    (l:ls) -> TextZipper ls l "" (a : la)
  Just (b', c) -> TextZipper lb b' (T.cons c a) la

right :: TextZipper -> TextZipper
right z@(TextZipper lb b a la) = case T.uncons a of
  Nothing -> case la of
    [] -> z
    (l:ls) -> TextZipper (b : lb) "" l ls
  Just (c, a') -> TextZipper lb (T.snoc b c) a' la

up :: TextZipper -> TextZipper
up z@(TextZipper lb b a la) = case lb of
  [] -> z
  (l:ls) ->
    let (b', a') = T.splitAt (T.length b) l
    in TextZipper ls b' a' ((b <> a) : la)

down :: TextZipper -> TextZipper
down z@(TextZipper lb b a la) = case la of
  [] -> z
  (l:ls) ->
    let (b', a') = T.splitAt (T.length b) l
    in TextZipper ((b <> a) : lb) b' a' ls

home :: TextZipper -> TextZipper
home (TextZipper lb b a la) = TextZipper lb "" (b <> a) la

end :: TextZipper -> TextZipper
end (TextZipper lb b a la) = TextZipper lb (b <> a) "" la

top :: TextZipper -> TextZipper
top (TextZipper lb b a la) = case reverse lb of
  [] -> TextZipper [] "" (b <> a) la
  (start:rest) -> TextZipper [] "" start (rest <> la)

insert :: Text -> TextZipper -> TextZipper
insert i z@(TextZipper lb b a la) = case T.split (=='\n') i of
  [] -> z
  (start:rest) -> case reverse rest of
    [] -> TextZipper lb (b <> start) a la
    (l:ls) -> TextZipper (ls <> [b <> start] <> lb) l a la

deleteLeft :: TextZipper-> TextZipper
deleteLeft z@(TextZipper lb b a la) = case T.unsnoc b of
  Nothing -> case lb of
    [] -> z
    (l:ls) -> TextZipper ls l a la
  Just (b', _) -> TextZipper lb b' a la

deleteRight :: TextZipper -> TextZipper
deleteRight z@(TextZipper lb b a la) = case T.uncons a of
  Nothing -> case la of
    [] -> z
    (l:ls) -> TextZipper lb b l ls
  Just (_, a') -> TextZipper lb b a' la

deleteLeftWord :: TextZipper -> TextZipper
deleteLeftWord (TextZipper lb b a la) =
  let b' = T.dropWhileEnd isSpace b
  in  if T.null b'
        then case lb of
          [] -> TextZipper [] b' a la
          (l:ls) -> deleteLeftWord $ TextZipper ls l a la
        else TextZipper lb (T.dropWhileEnd (not . isSpace) b') a la

value :: TextZipper -> Text
value (TextZipper lb b a la) = T.intercalate "\n" $ mconcat [ reverse lb
  , [b <> a]
  , la
  ]

empty :: TextZipper
empty = TextZipper [] "" "" []

fromText :: Text -> TextZipper
fromText = flip insert empty

-- | A span of text that makes up part of a display line
data Span = Span V.Attr Text
  deriving (Show)

-- | Default attributes for the text cursor
cursorAttributes :: V.Attr
cursorAttributes = V.withStyle V.defAttr V.reverseVideo

-- | Information about the documents as it is displayed (i.e., post-wrapping)
data DisplayLines = DisplayLines
  { _displayLines_spans :: [[Span]]
  , _displayLines_offsetMap :: Map Int Int
  , _displayLines_cursorY :: Int
  }
  deriving (Show)

-- | Given a width and a 'TextZipper', produce a list of display lines
-- (i.e., lines of wrapped text) with special attributes applied to
-- certain segments (e.g., the cursor). Additionally, produce the current
-- y-coordinate of the cursor and a mapping from display line number to text
-- offset
displayLines
  :: Int -- ^ Width, used for wrapping
  -> TextZipper -- ^ The text input contents and cursor state
  -> DisplayLines
displayLines width (TextZipper lb b a la) =
  let linesBefore :: [[Text]]
      linesBefore = map (wrapWithOffset width 0) $ reverse lb
      linesAfter :: [[Text]]
      linesAfter = map (wrapWithOffset width 0) la
      offsets = offsetMap $ mconcat
        [ linesBefore
        , [wrapWithOffset width 0 $ b <> a]
        , linesAfter
        ]
      spansBefore = map ((:[]) . Span V.defAttr) $ concat linesBefore
      spansAfter = map ((:[]) . Span V.defAttr) $ concat linesAfter
      (spansCurrentBefore, spansCurLineBefore) = fromMaybe ([], []) $
        initLast $ map ((:[]) . Span V.defAttr) (wrapWithOffset width 0 b)
      curLineOffset = spansLength spansCurLineBefore
      cursorAfterEOL = curLineOffset == width
      -- (cursor, spansCurLineAfter, spansCurrentAfter) = case T.uncons a of
      --   Nothing -> (Span cursorAttributes " ", [], [])
      --   Just (c, rest) ->
      --     ( Span cursorAttributes (T.singleton c)
      --     ,

      (spansCurLineAfter, spansCurrentAfter) = fromMaybe ([], []) $
        headTail $ case T.uncons a of
          Nothing -> [[Span cursorAttributes " "]]
          Just (c, rest) ->
            let o = if cursorAfterEOL then 1 else curLineOffset + 1
                cursor = Span cursorAttributes (T.singleton c)
            in  case map ((:[]) . Span V.defAttr) (wrapWithOffset width o rest) of
                  [] -> [[cursor]]
                  (l:ls) -> (cursor : l) : ls
  in  DisplayLines
        { _displayLines_spans = concat
          [ spansBefore
          , spansCurrentBefore
          , if cursorAfterEOL
              then [ spansCurLineBefore, spansCurLineAfter ]
              else [ spansCurLineBefore <> spansCurLineAfter ]
          , spansCurrentAfter
          , spansAfter
          ]
        , _displayLines_offsetMap = offsets
        , _displayLines_cursorY = sum
          [ length spansBefore
          , length spansCurrentBefore
          , if cursorAfterEOL then 1 else 0
          ]
        }
  where
    spansLength :: [Span] -> Int
    spansLength = sum . map (\(Span _ t) -> T.length t)
    initLast :: [a] -> Maybe ([a], a)
    initLast = \case
      [] -> Nothing
      (x:xs) -> case initLast xs of
        Nothing -> Just ([], x)
        Just (ys, y) -> Just (x:ys, y)
    headTail :: [a] -> Maybe (a, [a])
    headTail = \case
      [] -> Nothing
      x:xs -> Just (x, xs)

wrapWithOffset :: Int -> Int -> Text -> [Text]
wrapWithOffset maxWidth _ _ | maxWidth <= 0 = []
wrapWithOffset maxWidth n xs =
  let (firstLine, rest) = T.splitAt (maxWidth - n) xs
  in firstLine : (fmap (T.take maxWidth) . takeWhile (not . T.null) . iterate (T.drop maxWidth) $ rest)

-- | For a given set of wrapped logical lines, computes a map
-- from display line index to text offset in the original text.
-- This is used to help determine how interactions with the displayed
-- text map back to the original text.
-- For example, given the document @"AA\nBBB\nCCCCCCCC\n"@ wrapped to 5 columns,
-- this function will compute the offset in the original document of each character
-- in column 1:
-- @
--
--     AA...      (0, 0)
--     BBB..      (1, 3)
--     CCCCC      (2, 7)  -- (this line wraps to the next row)
--     CCC..      (3, 12)
--     .....      (4, 16)
--
--
-- @
offsetMap
  :: [[Text]] -- ^ The outer list represents logical lines, and the
              -- inner list represents the display lines into which
              -- the logical line has been wrapped
  -> Map Int Int -- ^ A map from the index (row) of display line to
                 -- the text offset from the beginning of the document
                 -- to the first character of the display line
offsetMap ts = evalState (offsetMap' ts) (0, 0)
  where
    offsetMap' xs = fmap Map.unions $ forM xs $ \x -> do
      maps <- forM x $ \line -> do
        let l = T.length line
        (dl, o) <- get
        put (dl + 1, o + l)
        return $ Map.singleton dl o
      (dl, o) <- get
      put (dl, o + 1)
      return $ Map.insert dl (o + 1) $ Map.unions maps

-- click :: (Int, Int) -> Int -> DisplayLines -> TextZipper
-- click (x, y) scrollTop dlines = undefined

images :: [[Span]] -> [V.Image]
images = map (V.horizCat . map spanToImage)

image :: [[Span]] -> V.Image
image = V.vertCat . images

spanToImage :: Span -> V.Image
spanToImage (Span attrs t) = V.text' attrs t

updateTextZipper :: V.Event -> TextZipper -> TextZipper
updateTextZipper ev = case ev of
  -- Regular characters
  V.EvKey (V.KChar k) [] -> insert $ T.singleton k
  -- Deletion buttons
  V.EvKey V.KBS [] -> deleteLeft
  V.EvKey V.KDel [] -> deleteRight
  -- Key combinations
  V.EvKey (V.KChar 'u') [V.MCtrl] -> const empty
  V.EvKey (V.KChar 'w') [V.MCtrl] -> deleteLeftWord
  -- Arrow keys
  V.EvKey V.KLeft [] -> left
  V.EvKey V.KRight [] -> right
  V.EvKey V.KUp [] -> up
  V.EvKey V.KDown [] -> down
  V.EvKey V.KHome [] -> home
  V.EvKey V.KEnd [] -> end
  _ -> id
