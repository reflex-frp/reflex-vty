{-# LANGUAGE FlexibleContexts #-}

-- | Explicit gaps and padding for core layout axes.
module Reflex.Vty.Widget.Layout.Spacing
  ( SpacingConfig (..)
  , layoutPadding
  , rowWithSpacing
  , columnWithSpacing
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Default (Default (..))
import Reflex

import qualified Reflex.Vty as Core
import Reflex.Vty.Style (Padding (..))
import Reflex.Vty.Widget.Layout

-- | Gap and padding configuration. Defaults are zero gap and zero
-- padding. Negative values are clamped to zero.
data SpacingConfig t = SpacingConfig
  { _spacingConfig_gap :: Dynamic t Int
  , _spacingConfig_padding :: Dynamic t Padding
  }

instance Reflex t => Default (SpacingConfig t) where
  def =
    SpacingConfig
      { _spacingConfig_gap = pure 0
      , _spacingConfig_padding = pure def
      }

-- | Run a child inside padding. The padding consists of fixed-size
-- spacers around the content in an outer column; the content keeps the
-- requested (or inherited) orientation. Negative values are clamped
-- to zero.
layoutPadding
  :: (Reflex t, MonadHold t m, MonadFix m, Core.HasLayout t m, Core.HasInput t m, Core.HasImageWriter t m, Core.HasDisplayRegion t m, Core.HasFocusReader t m)
  => Dynamic t Padding
  -> m a
  -> m a
layoutPadding padD child =
  Core.axis (pure Orientation_Column) Core.flex $ do
    Core.grout (fixed' (_padding_top <$> padD)) (pure ())
    mid <-
      Core.axis (pure Orientation_Row) Core.flex $ do
        Core.grout (fixed' (_padding_left <$> padD)) (pure ())
        a <- child
        Core.grout (fixed' (_padding_right <$> padD)) (pure ())
        pure a
    Core.grout (fixed' (_padding_bottom <$> padD)) (pure ())
    pure mid
  where
    fixed' = Core.fixed . fmap (max 0)

-- | Lay children out in a row with a fixed-size, nonfocusable gap
-- between consecutive children. No gap before the first or after the
-- last. Empty lists produce no content. Each list element is one
-- layout action (normally one 'tile' or 'grout').
rowWithSpacing
  :: (Reflex t, MonadHold t m, MonadFix m, Core.HasLayout t m, Core.HasInput t m, Core.HasImageWriter t m, Core.HasDisplayRegion t m, Core.HasFocusReader t m)
  => SpacingConfig t
  -> [m a]
  -> m [a]
rowWithSpacing = spacedWith Orientation_Row

-- | Lay children out in a column with gaps between consecutive
-- children.
columnWithSpacing
  :: (Reflex t, MonadHold t m, MonadFix m, Core.HasLayout t m, Core.HasInput t m, Core.HasImageWriter t m, Core.HasDisplayRegion t m, Core.HasFocusReader t m)
  => SpacingConfig t
  -> [m a]
  -> m [a]
columnWithSpacing = spacedWith Orientation_Column

spacedWith
  :: (Reflex t, MonadHold t m, MonadFix m, Core.HasLayout t m, Core.HasInput t m, Core.HasImageWriter t m, Core.HasDisplayRegion t m, Core.HasFocusReader t m)
  => Orientation
  -> SpacingConfig t
  -> [m a]
  -> m [a]
spacedWith o cfg kids =
  Core.axis (pure o) Core.flex $
    layoutPadding (_spacingConfig_padding cfg) $
      Core.axis (pure o) Core.flex (go kids)
  where
    gapD = Core.fixed (max 0 <$> _spacingConfig_gap cfg)
    go [] = pure []
    go [k] = (: []) <$> k
    go (k : rest) = do
      a <- k
      _ <- Core.grout gapD (pure ())
      as <- go rest
      pure (a : as)
