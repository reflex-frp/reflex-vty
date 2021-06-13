{-|
Module: Data.Text.Zipper
Description: A zipper for text documents that allows convenient editing and navigation

'TextZipper' is designed to be help manipulate the contents of a text input field. It keeps track of the logical lines of text (i.e., lines separated by user-entered newlines) and the current cursor position. Several functions are defined in this module to navigate and edit the TextZipper from the cursor position.

'TextZipper's can be converted into 'DisplayLines', which describe how the contents of the zipper will be displayed when wrapped to fit within a container of a certain width. It also provides some convenience facilities for converting interactions with the rendered DisplayLines back into manipulations of the underlying TextZipper.

-}
module Data.Text.Zipper where

import           Prelude

import Control.Exception (assert)
import Control.Monad.State (evalState, forM, get, put, join)
import Data.Char (isSpace)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import Data.Text.Internal (Text(..), text)
import Data.Text.Internal.Fusion (stream)
import Data.Text.Internal.Fusion.Types (Stream(..), Step(..))
import Data.Text.Unsafe
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T

import Graphics.Text.Width (wcwidth)


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

instance IsString TextZipper where
  fromString = fromText . T.pack

-- | Map a replacement function over the characters in a 'TextZipper'
mapZipper :: (Char -> Char) -> TextZipper -> TextZipper
mapZipper f (TextZipper lb b a la) = TextZipper
  { _textZipper_linesBefore = fmap (T.map f) lb
  , _textZipper_before = T.map f b
  , _textZipper_after = T.map f a
  , _textZipper_linesAfter = fmap (T.map f) la
  }

-- | Move the cursor left one character, if possible
left :: TextZipper -> TextZipper
left = leftN 1

-- | Move the cursor left by the given number of characters, or, if the document
-- isn't long enough, to the beginning of the document
leftN :: Int -> TextZipper -> TextZipper
leftN n z@(TextZipper lb b a la) =
  if T.length b >= n
    then
      let n' = T.length b - n
      in  TextZipper lb (T.take n' b) (T.drop n' b <> a) la
    else case lb of
           [] -> home z
           (l:ls) -> leftN (n - T.length b - 1) $ TextZipper ls l "" ((b <> a) : la)

-- | Move the cursor right one character, if possible
right :: TextZipper -> TextZipper
right = rightN 1

-- | Move the character right by the given number of characters, or, if the document
-- isn't long enough, to the end of the document
rightN :: Int -> TextZipper -> TextZipper
rightN n z@(TextZipper lb b a la) =
  if T.length a >= n
    then TextZipper lb (b <> T.take n a) (T.drop n a) la
    else case la of
           [] -> end z
           (l:ls) -> rightN (n - T.length a - 1) $ TextZipper ((b <> a) : lb) "" l ls

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

-- | Move the cursor up by the given number of lines
pageUp :: Int -> TextZipper -> TextZipper
pageUp pageSize z = if pageSize <= 0
  then z
  else pageUp (pageSize - 1) $ up z

-- | Move the cursor down by the given number of lines
pageDown :: Int -> TextZipper -> TextZipper
pageDown pageSize z = if pageSize <= 0
  then z
  else pageDown (pageSize - 1) $ down z

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
  (start:rest) -> TextZipper [] "" start (rest <> [b <> a] <> la)

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

-- | Insert up to n spaces to get to the next logical column that is a multiple of n
tab :: Int -> TextZipper -> TextZipper
tab n z@(TextZipper _ b _ _) =
  insert (T.replicate (fromEnum $ n - T.length b `mod` max 1 n) " ") z

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

-- | Text alignment type
data TextAlignment =
  TextAlignment_Left
  | TextAlignment_Right
  | TextAlignment_Center
  deriving (Eq, Show)

-- A map from the index (row) of display line to (fst,snd)
-- fst: leading empty spaces from left (may be negative) to adjust for alignment
-- snd: the text offset from the beginning of the document
-- to the first character of the display line
type OffsetMapWithAlignment = Map Int (Int, Int)

-- helper type representing a single visual line that may be part of a wrapped logical line
data WrappedLine = WrappedLine
  { _wrappedLines_text :: Text
  , _wrappedLines_hiddenWhitespace :: Bool -- ^ 'True' if this line ends with a deleted whitespace character
  , _wrappedLines_offset :: Int -- ^ offset from beginning of line
  }
  deriving (Eq, Show)

-- | Information about the document as it is displayed (i.e., post-wrapping)
data DisplayLines tag = DisplayLines
  { _displayLines_spans :: [[Span tag]]
  , _displayLines_offsetMap :: OffsetMapWithAlignment
  , _displayLines_cursorPos :: (Int, Int) -- cursor position relative to upper left hand corner
  }
  deriving (Show)

-- | Split a 'Text' at the given column index. For example
--
-- > splitAtWidth 3 "ᄀabc" == ("ᄀa", "bc")
--
-- because the first character has a width of two (see 'charWidth' for more on that).
splitAtWidth :: Int -> Text -> (Text, Text)
splitAtWidth n t@(Text arr off len)
    | n <= 0 = (T.empty, t)
    | n >= textWidth t = (t, T.empty)
    | otherwise = let k = toLogicalIndex n t
                  in (text arr off k, text arr (off+k) (len-k))

toLogicalIndex :: Int -> Text -> Int
toLogicalIndex n' t'@(Text _ _ len') = loop 0 0
  where loop !i !cnt
            | i >= len' || cnt + w > n' = i
            | otherwise = loop (i+d) (cnt + w)
          where Iter c d = iter t' i
                w = charWidth c

-- | Takes the given number of columns of characters. For example
--
-- > takeWidth 3 "ᄀabc" == "ᄀa"
--
-- because the first character has a width of 2 (see 'charWidth' for more on that).
-- This function will not take a character if its width exceeds the width it seeks to take.
takeWidth :: Int -> Text -> Text
takeWidth n = fst . splitAtWidth n

-- | Drops the given number of columns of characters. For example
--
-- > dropWidth 2 "ᄀabc" == "abc"
--
-- because the first character has a width of 2 (see 'charWidth' for more on that).
-- This function will not drop a character if its width exceeds the width it seeks to drop.
dropWidth :: Int -> Text -> Text
dropWidth n = snd . splitAtWidth n

-- | Get the display width of a 'Char'. "Full width" and "wide" characters
-- take two columns and everything else takes a single column. See
-- <https://www.unicode.org/reports/tr11/> for more information
-- This is implemented using wcwidth from Vty such that it matches what will
-- be displayed on the terminal. Note that this method can change depending
-- on how vty is configed. Please see vty documentation for details.
charWidth :: Char -> Int
charWidth = wcwidth

-- | Get the width of the text in a set of 'Span's, taking into account unicode character widths
spansWidth :: [Span tag] -> Int
spansWidth = sum . map (\(Span _ t) -> textWidth t)

-- | Get the length (number of characters) of the text in a set of 'Span's
spansLength :: [Span tag] -> Int
spansLength = sum . map (\(Span _ t) -> T.length t)

-- | Compute the width of some 'Text', taking into account fullwidth
-- unicode forms.
textWidth :: Text -> Int
textWidth t = widthI (stream t)

-- | Compute the width of a stream of characters, taking into account
-- fullwidth unicode forms.
widthI :: Stream Char -> Int
widthI (Stream next s0 _len) = loop_length 0 s0
    where
      loop_length !z s  = case next s of
                           Done       -> z
                           Skip    s' -> loop_length z s'
                           Yield c s' -> loop_length (z + charWidth c) s'
{-# INLINE[0] widthI #-}

-- | Compute the logical index position of a stream of characters from a visual
-- position taking into account fullwidth unicode forms.
charIndexAt :: Int -> Stream Char -> Int
charIndexAt pos (Stream next s0 _len) = loop_length 0 0 s0
    where
      loop_length i !z s  = case next s of
                           Done       -> i
                           Skip    s' -> loop_length i z s'
                           Yield c s' -> if w > pos then i else loop_length (i+1) w s' where
                             w = z + charWidth c
{-# INLINE[0] charIndexAt #-}




-- | Same as T.words except whitespace characters are included at end (i.e. ["line1 ", ...])
-- 'Char's representing white space.
wordsWithWhitespace :: Text -> [Text]
wordsWithWhitespace t@(Text arr off len) = loop 0 0 False
  where
    loop !start !n !wasSpace
        | n >= len = [Text arr (start+off) (n-start) | not (start == n)]
        | isSpace c = loop start (n+d) True
        | wasSpace = Text arr (start+off) (n-start) : loop n n False
        | otherwise = loop start (n+d) False
        where Iter c d = iter t n
{-# INLINE wordsWithWhitespace #-}

-- | Split words into logical lines, 'True' in the tuple indicates line ends with a whitespace character that got deleted
splitWordsAtDisplayWidth :: Int -> [Text] -> [(Text, Bool)]
splitWordsAtDisplayWidth maxWidth wwws = reverse $ loop wwws 0 [] where
  appendOut :: [(Text,Bool)] -> Text -> Bool -> [(Text,Bool)]
  appendOut [] t b           = [(t,b)]
  appendOut ((t',_):ts') t b = (t'<>t,b) : ts'

  -- remove the last whitespace in output
  modifyOutForNewLine :: [(Text,Bool)] -> [(Text,Bool)]
  modifyOutForNewLine [] = error "should never happen"
  modifyOutForNewLine ((t',_):ts) = case T.unsnoc t' of
    Nothing           -> error "should never happen"
    Just (t,lastChar) -> assert (isSpace lastChar) $ (t,True):ts -- assume last char is whitespace

  loop :: [Text] -> Int -> [(Text,Bool)] -> [(Text,Bool)]
  loop [] _ out = out
  loop (x:xs) cumw out = r where
    newWidth = textWidth x + cumw
    r = if newWidth > maxWidth
      then if isSpace $ T.index x (toLogicalIndex (maxWidth - cumw) x)
        -- if line runs over but character of splitting is whitespace then split on the whitespace
        then let (t1,t2) = splitAtWidth (maxWidth - cumw) x
          in loop (T.drop 1 t2:xs) 0 [] <> appendOut out t1 True
        else if cumw == 0
          -- single word exceeds max width, so just split on the word
          then let (t1,t2) = splitAtWidth (maxWidth - cumw) x
            in loop (t2:xs) 0 [] <> appendOut out t1 False
          -- otherwise start a new line
          else loop (x:xs) 0 [] <> modifyOutForNewLine out
      else loop xs newWidth $ appendOut out x False



-- | Wraps a logical line of text to fit within the given width. The first
-- wrapped line is offset by the number of columns provided. Subsequent wrapped
-- lines are not.
wrapWithOffsetAndAlignment
  :: TextAlignment
  -> Int -- ^ Maximum width
  -> Int -- ^ Offset for first line
  -> Text -- ^ Text to be wrapped
  -> [WrappedLine] -- (words on that line, hidden space char, offset from beginning of line)
wrapWithOffsetAndAlignment _ maxWidth _ _ | maxWidth <= 0 = []
wrapWithOffsetAndAlignment alignment maxWidth n txt = assert (n <= maxWidth) r where
  r' = splitWordsAtDisplayWidth maxWidth $ wordsWithWhitespace ( T.replicate n "." <> txt)
  fmapfn (t,b) = case alignment of
    TextAlignment_Left   -> WrappedLine t b 0
    TextAlignment_Right  -> WrappedLine t b (maxWidth-l)
    TextAlignment_Center -> WrappedLine t b ((maxWidth-l) `div` 2)
    where l = textWidth t
  r'' =  case r' of
    []       -> []
    (x,b):xs -> (T.drop n x,b):xs
  r = fmap fmapfn r''

-- converts deleted eol spaces into logical lines
eolSpacesToLogicalLines :: [[WrappedLine]] -> [[(Text, Int)]]
eolSpacesToLogicalLines = fmap (fmap (\(WrappedLine a _ c) -> (a,c))) . ((L.groupBy (\(WrappedLine _ b _) _ -> not b)) =<<)

offsetMapWithAlignmentInternal :: [[WrappedLine]] -> OffsetMapWithAlignment
offsetMapWithAlignmentInternal = offsetMapWithAlignment . eolSpacesToLogicalLines

offsetMapWithAlignment
  :: [[(Text, Int)]] -- ^ The outer list represents logical lines, inner list represents wrapped lines
  -> OffsetMapWithAlignment
offsetMapWithAlignment ts = evalState (offsetMap' ts) (0, 0)
  where
    offsetMap' xs = fmap Map.unions $ forM xs $ \x -> do
      maps <- forM x $ \(line,align) -> do
        let l = T.length line
        (dl, o) <- get
        put (dl + 1, o + l)
        return $ Map.singleton dl (align, o)
      (dl, o) <- get
      put (dl, o + 1)
      -- add additional offset to last line in wrapped lines (for newline char)
      return $ Map.adjust (\(align,_)->(align,o+1)) dl $ Map.unions maps


-- | Given a width and a 'TextZipper', produce a list of display lines
-- (i.e., lines of wrapped text) with special attributes applied to
-- certain segments (e.g., the cursor). Additionally, produce the current
-- y-coordinate of the cursor and a mapping from display line number to text
-- offset
displayLinesWithAlignment
  :: TextAlignment
  -> Int -- ^ Width, used for wrapping
  -> tag -- ^ Metadata for normal characters
  -> tag -- ^ Metadata for the cursor
  -> TextZipper -- ^ The text input contents and cursor state
  -> DisplayLines tag
displayLinesWithAlignment alignment width tag cursorTag (TextZipper lb b a la) =
  let linesBefore :: [[WrappedLine]] -- The wrapped lines before the cursor line
      linesBefore = map (wrapWithOffsetAndAlignment alignment width 0) $ reverse lb
      linesAfter :: [[WrappedLine]] -- The wrapped lines after the cursor line
      linesAfter = map (wrapWithOffsetAndAlignment alignment width 0) la
      offsets :: OffsetMapWithAlignment
      offsets = offsetMapWithAlignmentInternal $ mconcat
        [ linesBefore
        , [wrapWithOffsetAndAlignment alignment width 0 $ b <> a]
        , linesAfter
        ]
      flattenLines = concatMap (fmap _wrappedLines_text)
      spansBefore = map ((:[]) . Span tag) $ flattenLines linesBefore
      spansAfter = map ((:[]) . Span tag) $ flattenLines linesAfter
      -- Separate the spans before the cursor into
      -- * spans that are on earlier display lines (though on the same logical line), and
      -- * spans that are on the same display line

      -- TODO cursor goes here if align right
      (spansCurrentBefore, spansCurLineBefore) = fromMaybe ([], []) $
        initLast $ map ((:[]) . Span tag) $ _wrappedLines_text <$> (wrapWithOffsetAndAlignment alignment width 0 b)
      -- Calculate the number of columns on the cursor's display line before the cursor
      curLineOffset = spansWidth spansCurLineBefore
      -- Check whether the spans on the current display line are long enough that
      -- the cursor has to go to the next line
      cursorAfterEOL = curLineOffset == width
      cursorCharWidth = case T.uncons a of
        Nothing     -> 1
        Just (c, _) -> charWidth c

      -- Separate the span after the cursor into
      -- * spans that are on the same display line, and
      -- * spans that are on later display lines (though on the same logical line)

      -- TODO do not show cursor if align right
      (spansCurLineAfter, spansCurrentAfter) = fromMaybe ([], []) $
        headTail $ case T.uncons a of
          Nothing -> [[Span cursorTag " "]]
          Just (c, rest) ->
            let o = if cursorAfterEOL then cursorCharWidth else curLineOffset + cursorCharWidth
                cursor = Span cursorTag (T.singleton c)
            in case map ((:[]) . Span tag) $ _wrappedLines_text <$> (wrapWithOffsetAndAlignment alignment width o rest) of
                  []     -> [[cursor]]
                  (l:ls) -> (cursor : l) : ls

      curLineSpanNormalCase = if cursorAfterEOL
        then [ spansCurLineBefore, spansCurLineAfter ]
        else [ spansCurLineBefore <> spansCurLineAfter ]

      -- for right alignment, we want draw the cursor tag to be on the character just before the logical cursor position
      curLineSpan = if alignment == TextAlignment_Right && not cursorAfterEOL
        then case reverse spansCurLineBefore of
          [] -> curLineSpanNormalCase
          (Span _ x):xs -> case spansCurLineAfter of
            [] -> error "should not be possible" -- curLineSpanNormalCase
            (Span _ y):ys -> [reverse (Span cursorTag x:xs) <> ((Span tag y):ys)]
        else curLineSpanNormalCase

      cursorY = sum
        [ length spansBefore
        , length spansCurrentBefore
        , if cursorAfterEOL then 1 else 0
        ]
      -- a little silly to convert back to text but whatever, it works
      cursorX = if cursorAfterEOL then 0 else textWidth (mconcat $ fmap (\(Span _ t) -> t) spansCurLineBefore)

  in  DisplayLines
        { _displayLines_spans = concat
          [ spansBefore
          , spansCurrentBefore
          , curLineSpan
          , spansCurrentAfter
          , spansAfter
          ]
        , _displayLines_offsetMap = offsets
        , _displayLines_cursorPos = (cursorX, cursorY)
        }
  where
    initLast :: [a] -> Maybe ([a], a)
    initLast = \case
      [] -> Nothing
      (x:xs) -> case initLast xs of
        Nothing      -> Just ([], x)
        Just (ys, y) -> Just (x:ys, y)
    headTail :: [a] -> Maybe (a, [a])
    headTail = \case
      [] -> Nothing
      x:xs -> Just (x, xs)


-- | Move the cursor of the given 'TextZipper' to the logical position indicated
-- by the given display line coordinates, using the provided 'DisplayLinesWithAlignment'
-- information.  If the x coordinate is beyond the end of a line, the cursor is
-- moved to the end of the line.
goToDisplayLinePosition :: Int -> Int -> DisplayLines tag -> TextZipper -> TextZipper
goToDisplayLinePosition x y dl tz =
  let offset = Map.lookup y $ _displayLines_offsetMap dl
  in  case offset of
        Nothing -> tz
        Just (alignOff,o) ->
          let
            trueX = max 0 (x - alignOff)
            moveRight = case drop y $ _displayLines_spans dl of
                []    -> 0
                (s:_) -> charIndexAt trueX . stream . mconcat . fmap (\(Span _ t) -> t) $ s
          in  rightN (o + moveRight) $ top tz

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
displayLines = displayLinesWithAlignment TextAlignment_Left

-- | Wraps a logical line of text to fit within the given width. The first
-- wrapped line is offset by the number of columns provided. Subsequent wrapped
-- lines are not.
wrapWithOffset
  :: Int -- ^ Maximum width
  -> Int -- ^ Offset for first line
  -> Text -- ^ Text to be wrapped
  -> [Text]
wrapWithOffset maxWidth n xs = _wrappedLines_text <$> wrapWithOffsetAndAlignment TextAlignment_Left maxWidth n xs
