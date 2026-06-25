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
Sleep 800ms

# --- Todo List (button 1): tab down to the last task, then the
# "..." / "Profit!" gag (shows focus caret, insert-under-focus, cursor, Ctrl+T) ---
Tab
Sleep 250ms
Enter
Sleep 600ms
Tab@90ms 4
Sleep 350ms
Enter
Sleep 200ms
Type "..."
Sleep 400ms
Enter
Sleep 200ms
Type "Profit!"
Sleep 400ms
Ctrl+T
Sleep 800ms
Escape
Sleep 350ms

# --- Scrollbar modes (button 6): a Down-arrow scrolls all four panels at once,
# showing the gutter+thumb / thumb-only / while-scrolling / hidden styles ---
Tab@90ms 6
Sleep 200ms
Enter
Sleep 600ms
Down@80ms 12
Sleep 800ms
Escape
Sleep 350ms

# --- Showcase (button 8): styling, then cycle through themes with Tab (lots of color) ---
Tab@90ms 8
Sleep 200ms
Enter
Sleep 800ms
Tab
Sleep 650ms
Tab
Sleep 650ms
Tab
Sleep 650ms
Tab
Sleep 800ms

# --- Quit (hidden so the restored shell / nix output isn't recorded; the GIF
# ends on the showcase and loops cleanly back to the menu) ---
Hide
Ctrl+C
Sleep 500ms
