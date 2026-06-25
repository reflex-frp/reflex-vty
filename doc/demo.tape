# VHS tape for the reflex-vty README walkthrough GIF.
# Render with:  bash doc/record-demo.sh   (which builds the example first)
# Docs: https://github.com/charmbracelet/vhs

Output doc/reflex-vty.gif

Set Shell "bash"
# DejaVu Sans Mono is installed; VHS's default JetBrains Mono is not.
Set FontFamily "DejaVu Sans Mono"
Set FontSize 16
Set Width 1200
Set Height 760
Set Padding 20
Set Theme "Dracula"
Set TypingSpeed 60ms
# Allow plenty of time for the (hidden) nix-shell eval + app launch.
Set WaitTimeout 180s

# --- Launch (hidden so the build/run command and nix-shell startup aren't recorded).
# Wait until the menu actually appears before showing, so the nix build text is
# never recorded and keystrokes only land once the app is interactive. ---
Hide
Type "bash doc/run-example.sh"
Enter
Wait+Screen /Select an example/
Sleep 800ms
Show

# --- Main menu: the pink -> violet -> cyan gradient banner ---
Sleep 2s

# --- Todo List (button 1): focus indicators, insert-under-focus, visible cursor, Ctrl+T ---
Tab
Sleep 500ms
Enter
Sleep 1500ms
Enter
Sleep 500ms
Type "ship reflex-vty 1.0.0.0"
Sleep 1s
Ctrl+T
Sleep 1200ms
Escape
Sleep 800ms

# --- CPU Usage (button 5): the true-color vertical gradient bar ---
Tab@200ms 5
Sleep 400ms
Enter
Sleep 2500ms
Escape
Sleep 800ms

# --- Showcase (button 8): styling, then cycle themes with Tab ---
Tab@200ms 8
Sleep 400ms
Enter
Sleep 1800ms
Tab
Sleep 1300ms
Tab
Sleep 1300ms
Tab
Sleep 1300ms
Sleep 1s

# --- Quit (hidden so the restored shell / nix output isn't recorded; the GIF
# ends on the showcase and loops cleanly back to the menu) ---
Hide
Ctrl+C
Sleep 500ms
