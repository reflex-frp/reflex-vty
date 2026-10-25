# VHS tape for the Tier A widgets demo.
# Render with: nix-shell -p vhs ttyd ffmpeg chromium python3 --run 'vhs doc/widgets-demo.tape'
# Docs: https://github.com/charmbracelet/vhs

Output doc/widgets-demo.gif

Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 1160
Set Height 720
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
Sleep 1s

# Remap, disable (i passes into the field), then re-enable increment.
Type "xx"
Wait+Screen /count 2/
Sleep 800ms
Type "r"
Wait+Screen /i  increment counter/
Type "i"
Wait+Screen /count 3/
Sleep 1s
Type "d"
Wait+Screen /DISABLED/
Type "i"
Wait+Screen /Ai/
Sleep 1s
Type "d"
Type "i"
Wait+Screen /count 4/
Type "Z"
Wait+Screen /AiZ/
Sleep 800ms

# VHS lacks F-key commands and Type does not decode hex escapes:
# https://github.com/charmbracelet/vhs/blob/main/command.go
# https://github.com/charmbracelet/vhs/tree/main/lexer
# Recording transport only: Ctrl+F -> F6, Ctrl+G -> F7,
# Ctrl+O -> F8, Ctrl+T -> F9. Application bindings are unchanged.
Ctrl+F
Wait+Screen /C +D/
Sleep 1s
Type "+"
Sleep 600ms
Type "-"
Sleep 600ms
Ctrl+G
Sleep 800ms
Ctrl+O
Wait+Screen /1\/5/
Sleep 1s
Ctrl+Right
Wait+Screen /2\/5/
Sleep 800ms
Ctrl+Left
Wait+Screen /1\/5/
Ctrl+T
Wait+Screen /1\/1/
Sleep 800ms
Ctrl+T
Wait+Screen /0\/0/
Sleep 1s
Ctrl+T
Wait+Screen /1\/5/
Sleep 800ms

# The dialog owns printable keys; its nested popup owns the first Escape.
Type "m"
Wait+Screen /Tier A dialog/
Type "first"
Tab
Type "second"
Wait+Screen /Second: second/
Sleep 1s
Type "p"
Wait+Screen /Nested popover/
Sleep 1s
Escape
Wait+Screen /Escape: close dialog/
Sleep 1s
Escape
Wait+Screen /count 4/
Sleep 1s
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
