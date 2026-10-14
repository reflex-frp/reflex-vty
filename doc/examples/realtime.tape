# Render with: bash doc/record-example.sh realtime
Output doc/examples/realtime.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 220
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh realtime"
Enter
Wait+Screen /Simulated background activity/
Sleep 300ms
Show

Sleep 1s
Type "a"
Sleep 1s
Up
Sleep 1.5s

Hide
Type "q"
Sleep 300ms
