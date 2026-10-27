import Test.Hspec

import qualified Example.ArticlesSpec
import qualified Example.TierASpec
import qualified Reflex.Vty.Widget.GridSpec
import qualified Reflex.Vty.Widget.KeySpec
import qualified Reflex.Vty.Widget.Layout.SpacingSpec
import qualified Reflex.Vty.Widget.ListSpec
import qualified Reflex.Vty.Widget.OverlaySpec
import qualified Reflex.Vty.Widget.PaginatorSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Example.ArticlesSpec.spec
  Example.TierASpec.spec
  Reflex.Vty.Widget.GridSpec.spec
  Reflex.Vty.Widget.KeySpec.spec
  Reflex.Vty.Widget.Layout.SpacingSpec.spec
  Reflex.Vty.Widget.ListSpec.spec
  Reflex.Vty.Widget.OverlaySpec.spec
  Reflex.Vty.Widget.PaginatorSpec.spec
