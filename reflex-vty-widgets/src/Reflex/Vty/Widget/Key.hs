{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Reflex.Vty.Widget.Key
-- Description: Remappable key bindings with generated help
--
-- This module provides a small model of application key bindings
-- ('KeyMap', 'KeyBinding') together with widgets that dispatch input
-- events through such a map and render human-readable help from it.
--
-- Binding policy:
--
-- * Only 'V.EvKey' events dispatch, using core reflex-vty's exact
--   @(Key, [Modifier])@ equality. Extra modifiers never match.
-- * Earlier enabled bindings win conflicts; disabled bindings reserve
--   nothing. Repeated combinations within one record are deduplicated.
-- * 'keyEvents' observes without consuming. 'withKeyMap' removes only
--   the matched key events from its child's input; everything else
--   passes through to the child unchanged.
-- * Dispatch uses the map as of before the current frame: if a handler
--   remaps a binding in the same frame as the keystroke, that
--   keystroke still uses the old map and the next one uses the new.
module Reflex.Vty.Widget.Key
  ( KeyBinding (..)
  , KeyMap (..)

    -- * Model updates
  , remapKey
  , setBindingEnabled

    -- * Queries
  , effectiveBindings
  , dispatchTable
  , lookupKey

    -- * Rendering
  , keyComboText

    -- * Widgets
  , keyEvents
  , withKeyMap
  , keyHelp
  ) where

import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Default (Default (..))
import Data.List (foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex

import qualified Reflex.Vty as Core
import Data.Text.Zipper (TextAlignment (..))
import Reflex.Vty.Widget.Text (wrapTextImage)

-- | One application action together with the key combinations that
-- trigger it and a human-readable help string.
data KeyBinding action = KeyBinding
  { _keyBinding_action :: action
  , _keyBinding_keys :: [Core.KeyCombo]
  , _keyBinding_help :: Text
  , _keyBinding_enabled :: Bool
  }
  deriving (Eq, Functor, Show)

-- | An ordered collection of bindings. Earlier records win conflicts.
newtype KeyMap action = KeyMap
  { _keyMap_bindings :: [KeyBinding action]
  }
  deriving (Eq, Functor, Show)

instance Default (KeyMap action) where
  def = KeyMap []

-- | Replace the key combinations of every binding with the given
-- action. An empty replacement list unbinds the action. Bindings for
-- other actions, and each binding's help text, enabled state and
-- position, are unchanged. A missing action is left unchanged.
remapKey :: Eq action => action -> [Core.KeyCombo] -> KeyMap action -> KeyMap action
remapKey action keys (KeyMap bs) =
  KeyMap
    [ if _keyBinding_action b == action
        then b {_keyBinding_keys = keys}
        else b
    | b <- bs
    ]

-- | Enable or disable every binding with the given action. Missing
-- actions are left unchanged.
setBindingEnabled :: Eq action => action -> Bool -> KeyMap action -> KeyMap action
setBindingEnabled action enabled (KeyMap bs) =
  KeyMap
    [ if _keyBinding_action b == action
        then b {_keyBinding_enabled = enabled}
        else b
    | b <- bs
    ]

-- | The bindings that actually dispatch, in order: disabled records
-- are removed, combinations already claimed by an earlier record are
-- removed, and records left with no combinations are dropped.
-- Binding order and first-occurrence key order are preserved.
effectiveBindings :: KeyMap action -> [KeyBinding action]
effectiveBindings (KeyMap bs) = go bs Set.empty
  where
    go [] _ = []
    go (b : rest) claimed
      | not (_keyBinding_enabled b) = go rest claimed
      | otherwise =
          let keys' =
                [ k
                | k <- nub (_keyBinding_keys b)
                , not (Set.member k claimed)
                ]
           in case keys' of
                [] -> go rest claimed
                _ ->
                  b {_keyBinding_keys = keys'}
                    : go rest (foldl' (flip Set.insert) claimed keys')

-- | Build the dispatch table from the effective bindings. Earlier
-- records win, so the first occurrence of a combination is kept.
dispatchTable :: KeyMap action -> Map.Map Core.KeyCombo action
dispatchTable km =
  Map.fromList
    [ (k, _keyBinding_action b)
    | b <- effectiveBindings km
    , k <- _keyBinding_keys b
    ]

-- | Look up the action a key combination dispatches to. This agrees
-- with the dispatch table used by 'keyEvents' and 'withKeyMap'.
lookupKey :: KeyMap action -> Core.KeyCombo -> Maybe action
lookupKey = flip Map.lookup . dispatchTable

-- | A human-readable rendering of a key combination. Modifiers appear
-- in list order with @+@ separators. This is for display only; never
-- parse a formatted label back into a binding.
keyComboText :: Core.KeyCombo -> Text
keyComboText (k, mods) =
  T.intercalate "+" (map modifierText mods ++ [keyText k])
  where
    modifierText V.MShift = "Shift"
    modifierText V.MCtrl = "Ctrl"
    modifierText V.MMeta = "Meta"
    modifierText V.MAlt = "Alt"

    keyText :: V.Key -> Text
    keyText = \case
      V.KChar '\t' -> "Tab"
      V.KChar ' ' -> "Space"
      V.KChar c
        | isGraphicAscii c -> T.singleton c
        | otherwise -> T.pack (show c)
      V.KFun n -> T.pack ("F" ++ show n)
      V.KLeft -> "Left"
      V.KRight -> "Right"
      V.KUp -> "Up"
      V.KDown -> "Down"
      other -> T.pack (drop 1 (show other))

    -- Ordinary printable ASCII characters render literally; anything
    -- else that is a bare KChar uses Haskell 'show'.
    isGraphicAscii c = c >= '!' && c <= '~'

-- | The event of actions dispatched by key presses. This observes the
-- input without consuming it: other widgets still see all events.
keyEvents
  :: (Reflex t, Monad m, Core.HasInput t m)
  => Dynamic t (KeyMap action)
  -> m (Event t action)
keyEvents km = do
  i <- Core.input
  let table = dispatchTable <$> km
  pure $ attachWithMaybe lookupNow (current table) i
  where
    lookupNow tbl e = case e of
      V.EvKey k mods -> Map.lookup (k, mods) tbl
      _ -> Nothing

-- | Run a child widget whose matched key events are dispatched as
-- actions instead of reaching the child. Non-matching key events,
-- paste, mouse, resize and terminal-focus events pass through to the
-- child unchanged. Disabled or unbound keys are not consumed.
--
-- Like 'keyEvents', the map is sampled before the frame of each
-- keystroke.
withKeyMap
  :: (Reflex t, Monad m, Core.HasInput t m)
  => Dynamic t (KeyMap action)
  -> m a
  -> m (Event t action, a)
withKeyMap km child = do
  i <- Core.input
  let table = dispatchTable <$> km
      actions = attachWithMaybe lookupNow (current table) i
      childInput =
        attachWithMaybe
          ( \tbl e -> case e of
              V.EvKey k mods
                | Map.member (k, mods) tbl -> Nothing
              _ -> Just e
          )
          (current table)
          i
  a <- Core.localInput (const childInput) child
  pure (actions, a)
  where
    lookupNow tbl e = case e of
      V.EvKey k mods -> Map.lookup (k, mods) tbl
      _ -> Nothing

-- | Render one line per effective binding: the key combinations
-- joined with @\/@, two spaces, then the help text. Lines wrap to the
-- viewport with the ambient theme. Nothing is rendered for a
-- zero-area viewport or an empty map.
keyHelp
  :: forall t m action
   . (Reflex t, Monad m, Core.HasDisplayRegion t m, Core.HasImageWriter t m, Core.HasTheme t m)
  => Dynamic t (KeyMap action)
  -> m ()
keyHelp km = do
  attrsB <- Core.themeAttr
  w <- Core.viewportWidth
  h <- Core.viewportHeight
  Core.tellImages $
    ffor (current ((,,,) <$> km <*> w <*> h) <*> attrsB) $ \(km', wd, ht, attrs) ->
      let wd' = max 0 wd
          ht' = max 0 ht
          lines_ =
            [ T.intercalate " / " (map keyComboText (_keyBinding_keys b)) <> "  " <> _keyBinding_help b
            | b <- effectiveBindings km'
            ]
       in if wd' == 0 || ht' == 0 || null lines_
            then []
            else
              pure
                ( wrapTextImage TextAlignment_Left wd' attrs (T.intercalate "\n" lines_)
                )
