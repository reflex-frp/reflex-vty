# VHS tape for the Tier A widgets demo.
# Render with:  nix-shell -p vhs ttyd ffmpeg chromium --run 'vhs doc/widgets-demo.tape'
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

# --- Launch (hidden so the build/run command and nix-shell startup aren't
# recorded). Wait until the app is interactive before showing. ---
Hide
Type "nix-shell --run 'cabal run widgets-demo'"
Enter
Wait+Screen /increment/
Sleep 800ms
Show

# The rendered screen: generated key help, demo title.
Sleep 2s

# Quit cleanly via q (background-layer binding).
Type "q"
Sleep 1s
