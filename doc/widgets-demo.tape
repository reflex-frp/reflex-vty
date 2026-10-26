# VHS tape for the note demo: open, edit, cancel, reopen, save, quit.
# Render with: nix-shell -p vhs ttyd ffmpeg chromium python3 --run 'vhs doc/widgets-demo.tape'
# Docs: https://github.com/charmbracelet/vhs
#
# VHS has no caption/text-overlay command, so doc/widgets-demo-pty.py paints a
# three-row caption band below the demo's viewport. It only repaints between
# complete child escape/UTF-8 sequences and never after cleanup. Recording
# control bytes are consumed by the adapter and never reach the demo:
#   Ctrl+A open caption      Ctrl+B edit caption      Ctrl+D cancel caption
#   Ctrl+E reopen caption    Ctrl+F retype caption    Ctrl+G save caption
#   Ctrl+K quit caption
# Every recorded action uses the demo's real keys and typed text; there are no
# F-key transport mappings.

Output doc/widgets-demo.gif

Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 900
Set Height 540
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
Wait+Screen /Note: Ship on Friday/
Show
Sleep 2s

# Open the dialog with the real e binding. The caption band's intro scene
# already describes this step.
Type "e"
Wait+Screen /Edit note/
Wait+Screen /Enter: Save \| Escape: Cancel/
Sleep 1200ms

# The draft opens at its end: six Backspaces replace Friday with Monday.
Ctrl+B
Sleep 800ms
Backspace@60ms 6
Type "Monday"
Wait+Screen /Ship on Monday/
Sleep 2000ms

# Escape cancels. The committed background text must still say Friday.
Ctrl+D
Sleep 500ms
Escape
Wait+Screen /Note: Ship on Friday/
Wait+Screen /e: Edit \| q: Quit/
Sleep 1800ms

# Reopening starts the draft again from the committed Friday text.
Ctrl+E
Sleep 500ms
Type "e"
Wait+Screen /Edit note/
Wait+Screen /Ship on Friday/
Sleep 1800ms

# Edit again and save with Enter; the background becomes Monday.
Ctrl+F
Sleep 800ms
Backspace@60ms 6
Type "Monday"
Wait+Screen /Ship on Monday/
Ctrl+G
Sleep 800ms
Enter
Wait+Screen /Note: Ship on Monday/
Wait+Screen /e: Edit \| q: Quit/
Sleep 2000ms

# q quits from the background and the adapter restores the terminal.
Ctrl+K
Sleep 500ms
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
