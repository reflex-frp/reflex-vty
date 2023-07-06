{-|
  Description: Drawing boxes in various styles
-}
module Reflex.Vty.Widget.Box where

import Control.Monad.Fix (MonadFix)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty (Image)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Text

-- | Fill the background with the bottom box style
hRule :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => BoxStyle -> m ()
hRule boxStyle = fill $ pure (_boxStyle_s boxStyle)

-- | Defines a set of symbols to use to draw the outlines of boxes
-- C.f. https://en.wikipedia.org/wiki/Box-drawing_character
data BoxStyle = BoxStyle
  { _boxStyle_nw :: Char
  , _boxStyle_n :: Char
  , _boxStyle_ne :: Char
  , _boxStyle_e :: Char
  , _boxStyle_se :: Char
  , _boxStyle_s :: Char
  , _boxStyle_sw :: Char
  , _boxStyle_w :: Char
  }

instance Default BoxStyle where
  def = singleBoxStyle

-- | A box style that uses hyphens and pipe characters. Doesn't handle
-- corners very well.
hyphenBoxStyle :: BoxStyle
hyphenBoxStyle = BoxStyle '-' '-' '-' '|' '-' '-' '-' '|'

-- | A single line box style
singleBoxStyle :: BoxStyle
singleBoxStyle = BoxStyle '┌' '─' '┐' '│' '┘' '─' '└' '│'

-- | A thick single line box style
thickBoxStyle :: BoxStyle
thickBoxStyle = BoxStyle '┏' '━' '┓' '┃' '┛' '━' '┗' '┃'

-- | A double line box style
doubleBoxStyle :: BoxStyle
doubleBoxStyle = BoxStyle '╔' '═' '╗' '║' '╝' '═' '╚' '║'

-- | A single line box style with rounded corners
roundedBoxStyle :: BoxStyle
roundedBoxStyle = BoxStyle '╭' '─' '╮' '│' '╯' '─' '╰' '│'

-- | Draws a titled box in the provided style and a child widget inside of that box
boxTitle :: (MonadFix m, MonadHold t m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasFocusReader t m, HasTheme t m)
    => Behavior t BoxStyle
    -> Behavior t Text
    -> m a
    -> m a
boxTitle boxStyle title child = do
  dh <- displayHeight
  dw <- displayWidth
  bt <- theme
  let boxReg = Region 0 0 <$> dw <*> dh
      innerReg = Region 1 1 <$> (subtract 2 <$> dw) <*> (subtract 2 <$> dh)

  tellImages (boxImages <$> bt <*> title <*> boxStyle <*> current boxReg)
  tellImages (ffor2 (current innerReg) bt (\r attr -> [regionBlankImage attr r]))

  pane innerReg (pure True) child
  where
    boxImages :: V.Attr -> Text -> BoxStyle -> Region -> [Image]
    boxImages attr title' style (Region left top width height) =
      let right = left + width - 1
          bottom = top + height - 1
          sides =
            [ withinImage (Region (left + 1) top (width - 2) 1) $
                V.text' attr $
                  centerText title' (_boxStyle_n style) (width - 2)
            , withinImage (Region right (top + 1) 1 (height - 2)) $
                V.charFill attr (_boxStyle_e style) 1 (height - 2)
            , withinImage (Region (left + 1) bottom (width - 2) 1) $
                V.charFill attr (_boxStyle_s style) (width - 2) 1
            , withinImage (Region left (top + 1) 1 (height - 2)) $
                V.charFill attr (_boxStyle_w style) 1 (height - 2)
            ]
          corners =
            [ withinImage (Region left top 1 1) $
                V.char attr (_boxStyle_nw style)
            , withinImage (Region right top 1 1) $
                V.char attr (_boxStyle_ne style)
            , withinImage (Region right bottom 1 1) $
                V.char attr (_boxStyle_se style)
            , withinImage (Region left bottom 1 1) $
                V.char attr (_boxStyle_sw style)
            ]
      in sides ++ if width > 1 && height > 1 then corners else []

-- | Pad text  on the left and right with the given character so that it is
-- centered
centerText
  :: T.Text -- ^ Text to center
  -> Char -- ^ Padding character
  -> Int -- ^ Width
  -> T.Text -- ^ Padded text
centerText t c l = if lt >= l
                 then t
                 else left <> t <> right
  where
    lt = T.length t
    delta = l - lt
    mkHalf n = T.replicate (n `div` 2) (T.singleton c)
    left = mkHalf $ delta + 1
    right = mkHalf delta

-- | A box without a title
box :: (MonadFix m, MonadHold t m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasFocusReader t m, HasTheme t m)
    => Behavior t BoxStyle
    -> m a
    -> m a
box boxStyle = boxTitle boxStyle mempty

-- | A box whose style is static
boxStatic
  :: (MonadFix m, MonadHold t m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasFocusReader t m, HasTheme t m)
  => BoxStyle
  -> m a
  -> m a
boxStatic = box . pure
