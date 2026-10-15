-- |
-- Module: Reflex.Vty.Widget
-- Description: Umbrella re-export of the reflex-vty-widgets primitives
--
-- Re-exports the companion widget modules. This is not core
-- @Reflex.Vty.Widget@; import core modules directly for the base
-- widget vocabulary.
module Reflex.Vty.Widget
  ( module Reflex.Vty.Widget.Grid
  , module Reflex.Vty.Widget.Key
  , module Reflex.Vty.Widget.Layout.Spacing
  , module Reflex.Vty.Widget.Overlay
  , module Reflex.Vty.Widget.Paginator
  ) where

import Reflex.Vty.Widget.Grid
import Reflex.Vty.Widget.Key
import Reflex.Vty.Widget.Layout.Spacing
import Reflex.Vty.Widget.Overlay
import Reflex.Vty.Widget.Paginator
