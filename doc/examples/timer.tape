# Render with: bash doc/record-example.sh timer
Output doc/examples/timer.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 120
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh timer"
Enter
Wait+Screen /quit/
Sleep 300ms
Show

Sleep 4500ms

Hide
Type "q"
Sleep 300ms
