module Reflex.Vty.Widget.PaginatorSpec (spec) where

import Test.Hspec

import Reflex.Vty.Widget.Paginator

spec :: Spec
spec = describe "Reflex.Vty.Widget.Paginator model" $ do

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
