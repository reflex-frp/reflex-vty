# Render with: bash doc/record-example.sh fullscreen
Output doc/examples/fullscreen.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 300
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "export PS1='gallery> '"
Enter
Type "echo Return to this screen."
Enter
Wait+Screen /Return to this screen./
Sleep 300ms
Type "bash doc/run-examples.sh fullscreen"
Enter
Wait+Screen /This program exits in/
Sleep 300ms
Show

# The program exits on its own after five seconds; no quit key is sent.
Wait+Screen /Return to this screen./
Sleep 400ms

Hide
Sleep 200ms
