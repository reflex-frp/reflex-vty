# VHS tape for the Tier A widgets demo.
# Render with: nix-shell -p vhs ttyd ffmpeg chromium python3 --run 'vhs doc/widgets-demo.tape'
# Docs: https://github.com/charmbracelet/vhs
#
# VHS has no caption/text-overlay command (see its command reference and
# command.go), and its `Type` command does not decode hex escapes. The two
# recording-only pieces below keep the demo's own bindings untouched:
#
# * doc/widgets-demo-pty.py gives the child the same viewport as before and
#   reserves five outer terminal rows for a live caption band. It paints only
#   between complete child escape/UTF-8 sequences and never after cleanup.
# * Transport keys (recording controls, never application bindings):
#     Ctrl+F -> F6      Ctrl+G -> F7      Ctrl+O -> F8      Ctrl+T -> F9
#     Ctrl+A count 2    Ctrl+B type in grid    Ctrl+E F6 caption
#     Ctrl+K F8 caption Ctrl+L next page       Ctrl+N dialog caption
#     Ctrl+P form fields Ctrl+R popover        Ctrl+U first Escape caption
#     Ctrl+V second Escape Ctrl+W quit caption
#   The adapter strips the caption keys; they never reach the demo. The four
#   F-key mappings replace VHS's missing function-key commands; this tape
#   needs only Ctrl+F and Ctrl+O.

Output doc/widgets-demo.gif

Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 1160
# Five extra rows below the demo's viewport are reserved for captions; VHS
# resolves the exact terminal grid from the font metrics at render time.
Set Height 850
Set Padding 15
Set Theme "Dracula"
Set TypingSpeed 45ms
Set WaitTimeout 180s

# Prebuild widgets-demo. Keep startup and the exit-status check off camera.
Hide
Type `PS1='WIDGETS_DEMO_READY> '`
Enter
Wait+Line /^WIDGETS_DEMO_READY> *$/
Type `nix-shell --run 'python3 doc/widgets-demo-pty.py "$(cabal list-bin widgets-demo)"'`
Enter
Wait+Screen /count 0/
Show
Sleep 3s

# Root keys stay live while a grid field has focus: x twice increments the
# counter, and the focused cell is not edited. Hold the result to read both.
Type "xx"
Wait+Screen /count 2/
Ctrl+A
Sleep 2500ms

# Typing now goes to the focused cell. The F6 regroup must keep this text.
Ctrl+B
Sleep 300ms
Type "note"
Wait+Screen /Anote/
Sleep 2500ms

Ctrl+E
Sleep 300ms
Ctrl+F
Wait+Screen /Field C +Field D/
Sleep 2500ms

# The paginator is independent: only its representation and page change.
Ctrl+K
Sleep 300ms
Ctrl+O
Wait+Screen /1\/5/
Sleep 2500ms

Ctrl+L
Sleep 300ms
Ctrl+Right
Wait+Screen /2\/5/
Sleep 2500ms

# The dialog owns printable keys; the background counter stays at 2.
Ctrl+N
Sleep 300ms
Type "m"
Wait+Screen /Tier A dialog/
Sleep 2500ms

Ctrl+P
Sleep 300ms
Type "first"
Tab
Type "second"
Wait+Screen /Second: second/
Sleep 2500ms

# The popover is a read-only topmost layer: typing does not reach the form.
Ctrl+R
Sleep 300ms
Type "p"
Wait+Screen /Nested popover/
Sleep 2500ms

# First Escape closes only the popover; the dialog remains.
Ctrl+U
Sleep 300ms
Escape
Wait+Screen /Escape: close dialog/
Sleep 2500ms

# Second Escape closes the dialog and restores the background.
Ctrl+V
Sleep 300ms
Escape
Wait+Screen /count 2/
Sleep 2500ms

# q is background-only; the demo exits and the adapter restores the terminal.
Ctrl+W
Sleep 1500ms
Type "q"
Hide
Wait+Line /^WIDGETS_DEMO_READY> *$/
# VHS 0.11 reads Screen from buffer row 0, including old scrollback.
# Clear the hidden shell screen/scrollback without losing the demo's status.
Type `demo_status=$?; clear; (exit "$demo_status")`
Enter
Wait+Line /^WIDGETS_DEMO_READY> *$/
Type "echo WIDGETS_DEMO_EXIT=$?"
Enter
Wait+Screen /WIDGETS_DEMO_EXIT=0/
