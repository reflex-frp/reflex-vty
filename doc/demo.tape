# VHS tape for the reflex-vty README walkthrough GIF.
# Render with:  bash doc/record-demo.sh   (which builds the example first)
# Docs: https://github.com/charmbracelet/vhs

Output doc/reflex-vty.gif

Set Shell "bash"
# DejaVu Sans Mono is installed; VHS's default JetBrains Mono is not.
Set FontFamily "DejaVu Sans Mono"
# Bigger font fills the frame; width is kept wide enough that the showcase's
# two side-by-side columns don't collide (~94 cols), height trimmed to cut dead
# space in the simpler demos.
Set FontSize 20
Set Width 1160
Set Height 720
Set Padding 15
Set Theme "Dracula"
Set TypingSpeed 45ms
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
Sleep 1200ms

# --- Todo List (button 1): focus caret, toggle, insert-under-focus, visible cursor ---
Tab
Sleep 400ms
Enter
Sleep 1000ms
Ctrl+T
Sleep 800ms
Enter
Sleep 350ms
Type "ship reflex-vty 1.0.0.0"
Sleep 600ms
Enter
Sleep 350ms
Type "write the changelog"
Sleep 500ms
Ctrl+T
Sleep 1000ms
Escape
Sleep 600ms

# --- CPU Usage (button 5): the live true-color gradient bar (kept brief) ---
Tab@150ms 5
Sleep 300ms
Enter
Sleep 1500ms
Escape
Sleep 600ms

# --- Showcase (button 8): styling, then cycle through themes with Tab (lots of color) ---
Tab@150ms 8
Sleep 300ms
Enter
Sleep 1300ms
Tab
Sleep 1000ms
Tab
Sleep 1000ms
Tab
Sleep 1000ms
Tab
Sleep 1200ms

# --- Quit (hidden so the restored shell / nix output isn't recorded; the GIF
# ends on the showcase and loops cleanly back to the menu) ---
Hide
Ctrl+C
Sleep 500ms
