# ChangeLog

## 0.1.0.0

- Initial release: key bindings, layout spacing, grid, paginator, and overlay primitives.
- Fix paginator initialization from configured totals/page size, navigation and coincident parameter/command updates, explicit-set precedence, and the empty `0/0` indicator.
- Fix grid coordinates inside offset panes; cell state and focus survive regrouping.
- Complete overlay geometry, titled frames, panel/backdrop composition, input isolation, nested dismissal, deferred mouse-gesture visibility, and cached cursor restoration. Hidden and zero-area layers stay mounted without accepting new user activity.
- Replace the multi-widget showcase with a single-note editor: one text field in a dialog, Enter to save, Escape to discard, and a fresh draft from the saved note on every open. Background commands remain isolated from dialog typing.
- Replace placeholder overlay checks with recording-terminal scenarios and add real paginator, nested-grid, and production-demo regressions. Verify terminal input, resizing, quit routing, and restoration separately in direct PTY sessions.
- Add a recording-only PTY caption adapter with terminal restoration and child-status propagation; the tape checks normal exit status.
- Focus the recording on editing Friday to Monday, canceling, reopening the original, and then saving. Use a smaller canvas, short captions, and visible real editing keys; remove unused function-key transport mappings and unrelated showcase scenes.
- Add a keyed selectable list with keyboard and mouse selection, key-identity-preserving updates, and viewport following. Add an articles demo that opens a full-screen reading view and marks read articles. Parameterize the recording adapter with JSON scene files for the second demo.
