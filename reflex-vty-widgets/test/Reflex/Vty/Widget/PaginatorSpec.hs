{-# LANGUAGE PackageImports #-}
module Reflex.Vty.Widget.PaginatorSpec (spec) where

import Data.Default (def)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Reflex.Vty.Host (defaultVtyAppConfig)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..))
import "reflex-vty" Reflex.Vty.Widget
import Reflex.Vty.Widget.Paginator

spec :: Spec
spec = do
  describe "Reflex.Vty.Widget.Paginator model" $ do

    it "total 23 size 5 index 0 gives count 5 range (0,5)" $
      pageRange (makePageState 23 5 0) `shouldBe` (0, 5)

    it "last page gives index 4 range (20,23)" $ do
      let st = movePage PageLast (makePageState 23 5 0)
      pageIndex st `shouldBe` 4
      pageRange st `shouldBe` (20, 23)

    it "shrinking total to 3 clamps index to 0 range (0,3)" $ do
      let st = makePageState 3 5 4
      pageIndex st `shouldBe` 0
      pageRange st `shouldBe` (0, 3)

    it "zero items gives (0,0) and count 0" $ do
      let st = makePageState 0 10 2
      pageCount st `shouldBe` 0
      pageIndex st `shouldBe` 0
      pageRange st `shouldBe` (0, 0)

    it "negative totals and size zero are normalized" $ do
      let st = makePageState (-5) 0 3
      pageTotalItems st `shouldBe` 0
      pagePageSize st `shouldBe` 1
      pageCount st `shouldBe` 0

    it "navigation saturates without wrapping" $ do
      let st0 = makePageState 23 5 0
      pageIndex (movePage PagePrevious st0) `shouldBe` 0
      let stL = movePage PageLast st0
      pageIndex (movePage PageNext stL) `shouldBe` 4

    it "repeated previous at boundary stays at zero" $
      pageIndex (movePage PagePrevious (movePage PagePrevious (makePageState 23 5 1))) `shouldBe` 0

    it "explicit set out of range clamps" $
      pageIndex (movePage (PageSet 99) (makePageState 23 5 0)) `shouldBe` 4

    it "changing page size preserves index subject to clamping" $ do
      -- index 1 of 23@5 remains 1 when the size grows to 10 (count 3).
      let st = movePage PageNext (makePageState 23 5 0)
      pageIndex (makePageState 23 10 (pageIndex st)) `shouldBe` 1

  describe "Reflex.Vty.Widget.Paginator widget" $ do

    it "configured initial state renders 3/5 and reports range (10,15)" $
      withApp defaultVtyAppConfig (20, 3) (pgnApp False) $ \driver -> do
        f0 <- lastFrame driver
        frameObservation f0 `shouldBe` (2, 5, (10, 15))
        T.take 3 (T.pack (rowText 0 (frameGrid f0))) `shouldBe` "3/5"
        f1 <- stepInput driver (V.EvKey V.KPageDown [])
        frameObservation f1 `shouldBe` (3, 5, (15, 20))
        T.take 3 (T.pack (rowText 0 (frameGrid f1))) `shouldBe` "4/5"
        f2 <- stepCommand driver (PgnSet 99)
        frameObservation f2 `shouldBe` (4, 5, (20, 23))
        T.take 3 (T.pack (rowText 0 (frameGrid f2))) `shouldBe` "5/5"
        stopApp driver

    it "keyboard command advances after a parameter-only update" $
      withApp defaultVtyAppConfig (20, 3) (pgnApp False) $ \driver -> do
        -- A parameter-only frame settles the five-page state without
        -- carrying any command; the next keystroke must advance the
        -- page instead of being discarded.
        _ <- stepInput driver (V.EvKey (V.KChar 't') [])
        fAdv <- stepInput driver (V.EvKey V.KPageDown [])
        frameObservation fAdv `shouldBe` (3, 5, (15, 20))
        T.take 3 (T.pack (rowText 0 (frameGrid fAdv))) `shouldBe` "4/5"
        stopApp driver

    it "coincident parameter and command updates combine in one frame without later replays" $
      withApp defaultVtyAppConfig (20, 3) (pgnApp True) $ \driver -> do
        _ <- stepInput driver (V.EvKey V.KPageDown [])
        f4 <- stepInput driver (V.EvKey V.KPageDown [])
        frameObservation f4 `shouldBe` (4, 5, (20, 23))
        -- One keystroke carries both the shrink to total 7 and the
        -- keyboard PagePrevious: parameters normalize first, then the
        -- command moves from index 1 to 0.
        fShrunk <- stepInput driver (V.EvKey V.KLeft [])
        frameObservation fShrunk `shouldBe` (0, 2, (0, 5))
        T.take 3 (T.pack (rowText 0 (frameGrid fShrunk))) `shouldBe` "1/2"
        -- One keystroke carries total 13, keyboard PageLast, and an
        -- explicit set of 1; the explicit set wins over the keyboard.
        fSet <- stepInput driver (V.EvKey V.KEnd [])
        frameObservation fSet `shouldBe` (1, 3, (5, 10))
        T.take 3 (T.pack (rowText 0 (frameGrid fSet))) `shouldBe` "2/3"
        -- A later parameter-only frame must not replay PageLast or
        -- the explicit set.
        fParamOnly <- stepInput driver (V.EvKey (V.KChar 'v') [])
        frameObservation fParamOnly `shouldBe` (1, 3, (5, 10))
        -- A size-only change re-normalizes the same index.
        fSize <- stepInput driver (V.EvKey (V.KChar 's') [])
        frameObservation fSize `shouldBe` (1, 2, (10, 12))
        stopApp driver

    it "empty totals render 0/0 and restoring gives five pages at index 0" $
      withApp defaultVtyAppConfig (20, 3) (pgnApp False) $ \driver -> do
        _ <- stepInput driver (V.EvKey (V.KChar 'z') [])
        fEmpty <- lastFrame driver
        frameObservation fEmpty `shouldBe` (0, 0, (0, 0))
        T.take 3 (T.pack (rowText 0 (frameGrid fEmpty))) `shouldBe` "0/0"
        fRestored <- stepInput driver (V.EvKey (V.KChar 't') [])
        frameObservation fRestored `shouldBe` (0, 5, (0, 5))
        T.take 3 (T.pack (rowText 0 (frameGrid fRestored))) `shouldBe` "1/5"
        fLast <- stepInput driver (V.EvKey V.KEnd [])
        frameObservation fLast `shouldBe` (4, 5, (20, 23))
        stopApp driver

    it "dots mark the active page, fall back to numeric when narrow, and vanish at zero width" $
      withApp defaultVtyAppConfig (20, 1) dotsApp $ \driver -> do
        f0 <- lastFrame driver
        dotsOnly (frameGrid f0) `shouldBe` "●○○○○"
        fLast <- stepInput driver (V.EvKey V.KEnd [])
        frameObservation fLast `shouldBe` (4, 5, (20, 23))
        dotsOnly (frameGrid fLast) `shouldBe` "○○○○●"
        -- Nine dot columns do not fit in eight: numeric fallback.
        fNarrow <- resizeApp driver (8, 1)
        T.take 3 (T.pack (rowText 0 (frameGrid fNarrow))) `shouldBe` "5/5"
        -- A zero-area viewport emits no indicator while staying mounted.
        fZero <- resizeApp driver (0, 1)
        gridRows (frameGrid fZero) `shouldBe` []
        frameObservation fZero `shouldBe` (4, 5, (20, 23))
        fWide <- resizeApp driver (20, 1)
        dotsOnly (frameGrid fWide) `shouldBe` "○○○○●"
        stopApp driver

-- | External commands: explicit page-set requests.
data PgnCmd = PgnSet Int
  deriving (Eq, Show)

-- | The observed paginator state: index, page count, and item range.
type PgnObs = (Int, Int, (Int, Int))

-- | A numeric paginator over live totals. With @coincident@, some
-- terminal keys also update the totals (and End fires an explicit set
-- of 1) in the same frame as the paginator's own keyboard command, so
-- parameter and command updates genuinely coincide instead of
-- arriving in separate harness steps.
pgnApp :: Bool -> TestApp PgnCmd PgnObs
pgnApp coincident cmdEv = do
  i <- input
  let keyE k = ffilter (\e -> e == V.EvKey k []) i
      cmdSet = fforMaybe cmdEv $ \(PgnSet n) -> Just n
      totalE =
        leftmost
          ( [ 0 <$ keyE (V.KChar 'z')
            , 23 <$ keyE (V.KChar 't')
            ]
              ++ ( if coincident
                    then
                      [ 7 <$ keyE V.KLeft
                      , 13 <$ keyE V.KEnd
                      , 12 <$ keyE (V.KChar 'v')
                      ]
                    else []
                 )
          )
      sizeE = 10 <$ keyE (V.KChar 's')
      setE =
        leftmost
          ( cmdSet
              : [1 <$ keyE V.KEnd | coincident]
          )
  totalD <- holdDyn 23 totalE
  sizeD <- holdDyn 5 sizeE
  st <-
    paginator
      def
        { _paginatorConfig_totalItems = totalD
        , _paginatorConfig_pageSize = sizeD
        , _paginatorConfig_initialPage = 2
        , _paginatorConfig_setPage = setE
        , _paginatorConfig_keys = pure defaultPaginatorKeyMap
        , _paginatorConfig_display = pure PaginatorNumeric
        }
  pure (observe st, never)

-- | A dots paginator at a fixed 23 items / 5 per page, starting on
-- page 0, driven only by the default key map.
dotsApp :: TestApp PgnCmd PgnObs
dotsApp _ = do
  st <-
    paginator
      def
        { _paginatorConfig_totalItems = pure 23
        , _paginatorConfig_pageSize = pure 5
        , _paginatorConfig_keys = pure defaultPaginatorKeyMap
        }
  pure (observe st, never)

observe :: Reflex t => Dynamic t PageState -> Dynamic t PgnObs
observe st = (,,) <$> fmap pageIndex st <*> fmap pageCount st <*> fmap pageRange st

-- | Row text with only the spaces removed: width-agnostic comparison
-- for dot indicators.
dotsOnly :: Grid -> T.Text
dotsOnly g = T.filter (/= ' ') (T.pack (rowText 0 g))

lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame driver = do
  fs <- driverFrames driver
  case reverse fs of
    (f : _) -> pure f
    [] -> fail "expected at least one frame"

-- Local copy of the spec grid helper (GridSpec's copy is not shared).
rowText :: Int -> Grid -> String
rowText y g
  | y < 0 || y >= gridHeight g = ""
  | otherwise = T.unpack . T.concat . map renderSpanText $ gridRows g !! y
