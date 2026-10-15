import Test.Hspec

import qualified Reflex.Vty.Widget.KeySpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Reflex.Vty.Widget.KeySpec.spec
