import Test.Hspec

import qualified Reflex.Vty.Widget.GridSpec
import qualified Reflex.Vty.Widget.KeySpec
import qualified Reflex.Vty.Widget.Layout.SpacingSpec
import qualified Reflex.Vty.Widget.OverlaySpec
import qualified Reflex.Vty.Widget.PaginatorSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Reflex.Vty.Widget.GridSpec.spec
  Reflex.Vty.Widget.KeySpec.spec
  Reflex.Vty.Widget.Layout.SpacingSpec.spec
  Reflex.Vty.Widget.OverlaySpec.spec
  Reflex.Vty.Widget.PaginatorSpec.spec
