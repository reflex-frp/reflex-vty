-- | Pagination model and indicator views.
module Reflex.Vty.Widget.Paginator
  ( PageCommand (..)
  , PaginatorDisplay (..)
  , PageState
  , makePageState
  , pageCount
  , pageIndex
  , pageTotalItems
  , pagePageSize
  , movePage
  , pageRange
  , PaginatorConfig (..)
  , paginator
  , defaultPaginatorKeyMap
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex

import qualified Reflex.Vty as Core
import Reflex.Vty.Style (Style, applyAttr)
import Reflex.Vty.Widget.Key
  ( KeyBinding (..)
  , KeyMap (..)
  , dispatchTable
  , effectiveBindings
  )

-- * Model

data PageCommand = PagePrevious | PageNext | PageFirst | PageLast | PageSet Int
  deriving (Eq, Show)

data PaginatorDisplay = PaginatorDots | PaginatorNumeric
  deriving (Eq, Show)

-- | Abstract pagination state. 'Eq' and 'Show' derive from the internal
-- record; construct only through 'makePageState', 'movePage', or the
-- 'paginator' widget.
data PageState = PageState
  { _pageState_index :: !Int
  , _pageState_pageCount :: !Int
  , _pageState_totalItems :: !Int
  , _pageState_pageSize :: !Int
  }
  deriving (Eq, Show)

-- | Build a state from total items, page size, and initial page index.
-- Total is clamped to nonnegative and size to at least one; the index
-- is clamped into the valid range.
makePageState :: Int -> Int -> Int -> PageState
makePageState total size index =
  normalizeState (max 0 total) (max 1 size) index

normalizeState :: Int -> Int -> Int -> PageState
normalizeState total size index =
  let count = if total == 0 then 0 else 1 + ((total - 1) `div` size)
      idx = if count == 0 then 0 else max 0 (min (count - 1) index)
   in PageState idx count total size

-- | Read-only selectors.
pageCount, pageIndex, pageTotalItems, pagePageSize :: PageState -> Int
pageCount = _pageState_pageCount
pageIndex = _pageState_index
pageTotalItems = _pageState_totalItems
pagePageSize = _pageState_pageSize

-- | Apply one navigation command, saturating without wrapping.
movePage :: PageCommand -> PageState -> PageState
movePage cmd (PageState idx count total size) =
  let idx' = case cmd of
        PagePrevious -> max 0 (idx - 1)
        PageNext -> min (count - 1) (idx + 1)
        PageFirst -> 0
        PageLast -> max 0 (count - 1)
        PageSet i -> i
   in normalizeState total size idx'

-- | Half-open item bounds of the current page. Zero items give
-- @(0, 0)@.
pageRange :: PageState -> (Int, Int)
pageRange (PageState idx count total size)
  | count == 0 = (0, 0)
  | otherwise =
      let start = idx * size
          end = start + min size (total - start)
       in (start, end)

-- * Widget

-- | Configuration for the paginator indicator.
data PaginatorConfig t = PaginatorConfig
  { _paginatorConfig_totalItems :: Dynamic t Int
  , _paginatorConfig_pageSize :: Dynamic t Int
  , _paginatorConfig_initialPage :: Int
  , _paginatorConfig_setPage :: Event t Int
  , _paginatorConfig_keys :: Dynamic t (KeyMap PageCommand)
  , _paginatorConfig_display :: Behavior t PaginatorDisplay
  , _paginatorConfig_activeStyle :: Behavior t Style
  , _paginatorConfig_inactiveStyle :: Behavior t Style
  }

instance Reflex t => Default (PaginatorConfig t) where
  def =
    PaginatorConfig
      { _paginatorConfig_totalItems = pure 0
      , _paginatorConfig_pageSize = pure 10
      , _paginatorConfig_initialPage = 0
      , _paginatorConfig_setPage = never
      , _paginatorConfig_keys = pure def
      , _paginatorConfig_display = pure PaginatorDots
      , _paginatorConfig_activeStyle = pure def
      , _paginatorConfig_inactiveStyle = pure def
      }

-- | Default navigation bindings with help strings.
defaultPaginatorKeyMap :: KeyMap PageCommand
defaultPaginatorKeyMap =
  KeyMap
    [ KeyBinding PagePrevious [(V.KLeft, []), (V.KPageUp, [])] "previous page" True
    , KeyBinding PageNext [(V.KRight, []), (V.KPageDown, [])] "next page" True
    , KeyBinding PageFirst [(V.KHome, [])] "first page" True
    , KeyBinding PageLast [(V.KEnd, [])] "last page" True
    ]

-- | One frame's update: new parameters first (the prior index is
-- re-normalized against them), then a command. 'Nothing' means no
-- change to that part.
data FrameUpdate = FrameUpdate
  { _frameUpdate_params :: Maybe (Int, Int)
  , _frameUpdate_command :: Maybe PageCommand
  }

-- | The paginator indicator. Returns the live state; callers render
-- their page content from 'pageRange'.
--
-- The key map is sampled before the frame of each keystroke. An
-- explicit '_paginatorConfig_setPage' in the same frame as a keyboard
-- command wins. Parameter updates (total/size) are sampled promptly
-- for the frame that carries them.
paginator
  :: forall t m
   . ( Reflex t, MonadHold t m, MonadFix m, Core.HasInput t m
     , Core.HasDisplayRegion t m, Core.HasImageWriter t m, Core.HasTheme t m
     )
  => PaginatorConfig t
  -> m (Dynamic t PageState)
paginator cfg = do
  i <- Core.input
  let keyActions =
        attachWithMaybe
          ( \km e -> case e of
              V.EvKey k mods -> Map.lookup (k, mods) (dispatchTable km)
              _ -> Nothing
          )
          (current (_paginatorConfig_keys cfg))
          i
      -- Parameter changes, sampled promptly for their frame.
      paramPairs =
        zipDyn (_paginatorConfig_totalItems cfg) (_paginatorConfig_pageSize cfg)
      paramE = updated paramPairs
      -- Explicit set wins over keyboard command in the same frame.
      setE = PageSet <$> _paginatorConfig_setPage cfg
      cmdE = leftmost [setE, keyActions]
      -- Both a parameter update and a command in one frame: parameters
      -- first (normalize), then the command.
      -- Both a parameter update and a command can fire in the same
      -- frame; combine them with a prompt sampling of the command.
      frameE =
        attachWithMaybe
          ( \mc e ->
              let cmd = case mc of
                    Just c -> Just c
                    Nothing -> Nothing
               in case e of
                    Just p -> Just (FrameUpdate (Just p) cmd)
                    Nothing -> fmap (FrameUpdate Nothing . Just) cmd
          )
          (current (pure Nothing))
          (leftmost [Just <$> paramE, Nothing <$ cmdE])
      _ = ()
  stateD <- foldDyn applyFrame (makePageState 0 10 (_paginatorConfig_initialPage cfg)) frameE
  -- Render the one-row indicator.
  vpW <- Core.viewportWidth
  vpH <- Core.viewportHeight
  Core.tellImages $
    ffor
      ( (,,,)
          <$> ((,,) <$> current stateD <*> current vpW <*> current vpH)
          <*> _paginatorConfig_display cfg
          <*> _paginatorConfig_activeStyle cfg
          <*> _paginatorConfig_inactiveStyle cfg
      )
      ( \((st, wd, ht), disp, act, inact) ->
          renderIndicator st (wd, ht) disp (applyAttr act V.defAttr) (applyAttr inact V.defAttr)
      )
  pure stateD
  where
    renderIndicator st (wd, ht) disp act inact
      | wd <= 0 || ht <= 0 = []
      | otherwise =
          let display
                | pageCount st == 0 = PaginatorNumeric
                | otherwise = disp
              -- A dot row needs 2*count-1 columns; compare without
              -- constructing the row.
              fitsDots = 2 * pageCount st - 1 <= wd
              disp' = case display of
                PaginatorDots | not fitsDots -> PaginatorNumeric
                d -> d
           in case disp' of
                PaginatorNumeric ->
                  let row = T.pack (show (pageIndex st + 1) ++ "/" ++ show (pageCount st))
                   in [V.text' act (T.take wd row)]
                PaginatorDots ->
                  [ V.crop wd ht $ V.horizCat $
                      concatMap
                        ( \p ->
                            (if p == 0 then [] else [V.char inact ' '])
                              ++ [ V.char
                                    (if p == pageIndex st then act else inact)
                                    (if p == pageIndex st then '●' else '○')
                                 ]
                        )
                        [0 .. pageCount st - 1]
                  ]
    applyFrame (FrameUpdate mp mc) st =
      let st' = case mp of
            Just (total, size) ->
              normalizeState (max 0 total) (max 1 size) (pageIndex st)
            Nothing -> st
       in case mc of
            Just c -> movePage c st'
            Nothing -> st'

