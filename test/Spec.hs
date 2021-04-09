-- hspec auto-discovery stuff
-- does not work with current CircleCI image so instead we do it manually for now
-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}


import Test.Hspec

import qualified Data.Text.ZipperSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.Text.ZipperSpec"     Data.Text.ZipperSpec.spec
