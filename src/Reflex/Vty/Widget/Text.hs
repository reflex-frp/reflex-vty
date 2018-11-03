{-# LANGUAGE OverloadedStrings #-}
module Reflex.Vty.Widget.Text where

import Data.Text (Text)
import qualified Data.Text as T

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

value :: TextZipper -> Text
value (TextZipper lb b a la) = T.intercalate "\n" $ mconcat
  [ reverse lb
  , [b <> a]
  , la
  ]

empty :: TextZipper
empty = TextZipper [] "" "" []
