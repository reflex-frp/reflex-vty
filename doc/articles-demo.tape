# VHS tape for the articles demo: select, read, return, read the marker, quit.
# Render with: nix-shell -p vhs ttyd ffmpeg chromium python3 --run 'vhs doc/articles-demo.tape'
# Docs: https://github.com/charmbracelet/vhs
#
# doc/widgets-demo-pty.py paints the caption band from
# doc/articles-demo-scenes.json. Recording control bytes are consumed by the
# adapter and never reach the demo:
#   Ctrl+A read caption   Ctrl+B return caption
#   Ctrl+D next-article caption   Ctrl+E quit caption
# Every recorded action uses the demo's real keys: Down, Enter, Escape, q.

Output doc/articles-demo.gif

Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 900
Set Height 540
Set Padding 15
Set Theme "Dracula"
Set TypingSpeed 45ms
Set WaitTimeout 180s

# Prebuild articles-demo. Keep startup and the exit-status check off camera.
Hide
Type `PS1='ARTICLES_DEMO_READY> '`
Enter
Wait+Line /^ARTICLES_DEMO_READY> *$/
Type `nix-shell --run 'python3 doc/widgets-demo-pty.py doc/articles-demo-scenes.json "$(cabal list-bin articles-demo)"'`
Enter
Wait+Screen /Articles/
Wait+Screen /● Reflex 1.3 released/
Wait+Screen /● Terminal scrolling deep dive/
Wait+Screen /● Grid layout tips/
Wait+Screen /Enter: Read \| q: Quit/
Show
Sleep 2s

# Intro scene: Down only moves the highlight; no article opens.
Down
Sleep 1200ms

# Enter opens the highlighted second article.
Ctrl+A
Sleep 800ms
Enter
Wait+Screen /Terminal scrolling deep dive/
Wait+Screen /Escape: Back/
Sleep 2600ms

# Escape returns; the hollow bullet marks the article read.
Ctrl+B
Sleep 800ms
Escape
Wait+Screen /Enter: Read \| q: Quit/
Wait+Screen /○ Terminal scrolling deep dive/
Sleep 2600ms

# Selection was preserved; one Down reaches the third article.
Ctrl+D
Sleep 800ms
Down
Sleep 800ms
Enter
Wait+Screen /Grid layout tips/
Wait+Screen /Escape: Back/
Sleep 1400ms
Escape
Wait+Screen /Enter: Read \| q: Quit/
Wait+Screen /○ Grid layout tips/
Sleep 1000ms

# Stop recording before q restores the pre-demo terminal.
Ctrl+E
Sleep 800ms
Hide
Type "q"
Wait+Line /^ARTICLES_DEMO_READY> *$/
# VHS 0.11 reads Screen from buffer row 0, including old scrollback.
# Clear the hidden shell screen/scrollback without losing the demo's status.
Type `demo_status=$?; clear; (exit "$demo_status")`
Enter
Wait+Line /^ARTICLES_DEMO_READY> *$/
Type "echo ARTICLES_DEMO_EXIT=$?"
Enter
Wait+Screen /ARTICLES_DEMO_EXIT=0/
