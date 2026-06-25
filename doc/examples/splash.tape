# Render with: bash doc/record-example.sh splash
Output doc/examples/splash.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 18
Set Width 680
Set Height 240
Set Padding 6
Set Theme "Dracula"
Set Framerate 24
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh splash"
Enter
Wait+Screen /reflex/
Sleep 300ms
Show

Sleep 5500ms

Hide
Type "q"
Sleep 300ms
