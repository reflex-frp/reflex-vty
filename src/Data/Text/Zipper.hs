{-|
Module: Data.Text.Zipper
Description: A zipper for text documents that allows convenient editing and navigation

'TextZipper' is designed to be help manipulate the contents of a text input field. It keeps track of the logical lines of text (i.e., lines separated by user-entered newlines) and the current cursor position. Several functions are defined in this module to navigate and edit the TextZipper from the cursor position.

'TextZipper's can be converted into 'DisplayLines', which describe how the contents of the zipper will be displayed when wrapped to fit within a container of a certain width. It also provides some convenience facilities for converting interactions with the rendered DisplayLines back into manipulations of the underlying TextZipper.

-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Zipper where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State

import Data.Text (Text)
import qualified Data.Text as T

-- | A zipper of the logical text input contents (the "document"). The lines
-- before the line containing the cursor are stored in reverse order.
-- The cursor is logically between the "before" and "after" text.
-- A "logical" line of input is a line of input up until a user-entered newline
-- character (as compared to a "display" line, which is wrapped to fit within
-- a given viewport width).
data TextZipper = TextZipper
  { _textZipper_linesBefore :: [Text] -- reversed
  , _textZipper_before :: Text
  , _textZipper_after :: Text -- The cursor is on top of the first character of this text
  , _textZipper_linesAfter :: [Text]
  }
  deriving (Show)

-- | Move the cursor left one character, if possible
left :: TextZipper -> TextZipper
left z@(TextZipper lb b a la) = case T.unsnoc b of
  Nothing -> case lb of
    [] -> z
    (l:ls) -> TextZipper ls l "" (a : la)
  Just (b', c) -> TextZipper lb b' (T.cons c a) la

-- | Move the cursor right one character, if possible
right :: TextZipper -> TextZipper
right z@(TextZipper lb b a la) = case T.uncons a of
  Nothing -> case la of
    [] -> z
    (l:ls) -> TextZipper (b : lb) "" l ls
  Just (c, a') -> TextZipper lb (T.snoc b c) a' la

-- | Move the cursor up one logical line, if possible
up :: TextZipper -> TextZipper
up z@(TextZipper lb b a la) = case lb of
  [] -> z
  (l:ls) ->
    let (b', a') = T.splitAt (T.length b) l
    in TextZipper ls b' a' ((b <> a) : la)

-- | Move the cursor down one logical line, if possible
down :: TextZipper -> TextZipper
down z@(TextZipper lb b a la) = case la of
  [] -> z
  (l:ls) ->
    let (b', a') = T.splitAt (T.length b) l
    in TextZipper ((b <> a) : lb) b' a' ls

-- | Move the cursor to the beginning of the current logical line
home :: TextZipper -> TextZipper
home (TextZipper lb b a la) = TextZipper lb "" (b <> a) la

-- | Move the cursor to the end of the current logical line
end :: TextZipper -> TextZipper
end (TextZipper lb b a la) = TextZipper lb (b <> a) "" la

-- | Move the cursor to the top of the document
top :: TextZipper -> TextZipper
top (TextZipper lb b a la) = case reverse lb of
  [] -> TextZipper [] "" (b <> a) la
  (start:rest) -> TextZipper [] "" start (rest <> la)

-- | Insert a character at the current cursor position
insertChar :: Char -> TextZipper -> TextZipper
insertChar i = insert (T.singleton i)

-- | Insert text at the current cursor position
insert :: Text -> TextZipper -> TextZipper
insert i z@(TextZipper lb b a la) = case T.split (=='\n') i of
  [] -> z
  (start:rest) -> case reverse rest of
    [] -> TextZipper lb (b <> start) a la
    (l:ls) -> TextZipper (ls <> [b <> start] <> lb) l a la

-- | Delete the character to the left of the cursor
deleteLeft :: TextZipper-> TextZipper
deleteLeft z@(TextZipper lb b a la) = case T.unsnoc b of
  Nothing -> case lb of
    [] -> z
    (l:ls) -> TextZipper ls l a la
  Just (b', _) -> TextZipper lb b' a la

-- | Delete the character under/to the right of the cursor
deleteRight :: TextZipper -> TextZipper
deleteRight z@(TextZipper lb b a la) = case T.uncons a of
  Nothing -> case la of
    [] -> z
    (l:ls) -> TextZipper lb b l ls
  Just (_, a') -> TextZipper lb b a' la

-- | Delete a word to the left of the cursor. Deletes all whitespace until it
-- finds a non-whitespace character, and then deletes contiguous non-whitespace
-- characters.
deleteLeftWord :: TextZipper -> TextZipper
deleteLeftWord (TextZipper lb b a la) =
  let b' = T.dropWhileEnd isSpace b
  in  if T.null b'
        then case lb of
          [] -> TextZipper [] b' a la
          (l:ls) -> deleteLeftWord $ TextZipper ls l a la
        else TextZipper lb (T.dropWhileEnd (not . isSpace) b') a la

-- | The plain text contents of the zipper
value :: TextZipper -> Text
value (TextZipper lb b a la) = T.intercalate "\n" $ mconcat [ reverse lb
  , [b <> a]
  , la
  ]

-- | The empty zipper
empty :: TextZipper
empty = TextZipper [] "" "" []

-- | Constructs a zipper with the given contents. The cursor is placed after
-- the contents.
fromText :: Text -> TextZipper
fromText = flip insert empty

-- | A span of text tagged with some metadata that makes up part of a display
-- line.
data Span tag = Span tag Text
  deriving (Show)

-- | Information about the document as it is displayed (i.e., post-wrapping)
data DisplayLines tag = DisplayLines
  { _displayLines_spans :: [[Span tag]]
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
  -> tag -- ^ Metadata for normal characters
  -> tag -- ^ Metadata for the cursor
  -> TextZipper -- ^ The text input contents and cursor state
  -> DisplayLines tag
displayLines width tag cursorTag (TextZipper lb b a la) =
  let linesBefore :: [[Text]]
      linesBefore = map (wrapWithOffset width 0) $ reverse lb
      linesAfter :: [[Text]]
      linesAfter = map (wrapWithOffset width 0) la
      offsets = offsetMap $ mconcat
        [ linesBefore
        , [wrapWithOffset width 0 $ b <> a]
        , linesAfter
        ]
      spansBefore = map ((:[]) . Span tag) $ concat linesBefore
      spansAfter = map ((:[]) . Span tag) $ concat linesAfter
      (spansCurrentBefore, spansCurLineBefore) = fromMaybe ([], []) $
        initLast $ map ((:[]) . Span tag) (wrapWithOffset width 0 b)
      curLineOffset = spansLength spansCurLineBefore
      cursorAfterEOL = curLineOffset == width
      (spansCurLineAfter, spansCurrentAfter) = fromMaybe ([], []) $
        headTail $ case T.uncons a of
          Nothing -> [[Span cursorTag " "]]
          Just (c, rest) ->
            let o = if cursorAfterEOL then 1 else curLineOffset + 1
                cursor = Span cursorTag (T.singleton c)
            in  case map ((:[]) . Span tag) (wrapWithOffset width o rest) of
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
    spansLength :: [Span tag] -> Int
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

-- | Wraps a logical line of text to fit within the given width. The first
-- wrapped line is offset by the number of columns provided. Subsequent wrapped
-- lines are not.
wrapWithOffset :: Int -> Int -> Text -> [Text]
wrapWithOffset maxWidth _ _ | maxWidth <= 0 = []
wrapWithOffset maxWidth n xs =
  let (firstLine, rest) = T.splitAt (maxWidth - n) xs
  in firstLine : (fmap (T.take maxWidth) . takeWhile (not . T.null) . iterate (T.drop maxWidth) $ rest)

-- | For a given set of wrapped logical lines, computes a map
-- from display line index to text offset in the original text.
-- This is used to help determine how interactions with the displayed
-- text map back to the original text.
-- For example, given the document @\"AA\\nBBB\\nCCCCCCCC\\n\"@ wrapped to 5 columns,
-- this function will compute the offset in the original document of each character
-- in column 1:
--
-- >   AA...      (0, 0)
-- >   BBB..      (1, 3)
-- >   CCCCC      (2, 7)  -- (this line wraps to the next row)
-- >   CCC..      (3, 12)
-- >   .....      (4, 16)
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
