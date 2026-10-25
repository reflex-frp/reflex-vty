# ChangeLog

## 0.1.0.0

- Initial release: key bindings, layout spacing, grid, paginator, and overlay primitives.
- Fix paginator initialization from configured totals/page size, navigation and coincident parameter/command updates, explicit-set precedence, and the empty `0/0` indicator.
- Fix grid coordinates inside offset panes; cell state and focus survive regrouping.
- Complete overlay geometry, titled frames, panel/backdrop composition, input isolation, nested dismissal, deferred mouse-gesture visibility, and cached cursor restoration. Hidden and zero-area layers stay mounted without accepting new user activity.
- Repair the Tier A demo's dispatched controls and enabled bindings; allocate separate help, grid, paginator, and hint rows, with a persistent two-field dialog and nested read-only popover.
- Replace placeholder overlay checks with recording-terminal scenarios and add real paginator, nested-grid, and production-demo regressions. Verify terminal input, resizing, quit routing, and restoration separately in direct PTY sessions.
- Replace the incomplete demo recording with the full remap/disable, layout, pagination, dialog, and nested-dismissal sequence. A recording-only PTY adapter supplies F6–F9 for VHS, restores terminal modes, and propagates child failures; the tape checks normal exit status.
