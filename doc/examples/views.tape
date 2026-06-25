# Render with: bash doc/record-example.sh views
Output doc/examples/views.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 180
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh views"
Enter
Wait+Screen /switch/
Sleep 300ms
Show

Sleep 800ms
Tab
Sleep 1100ms
Tab
Sleep 1100ms
Tab
Sleep 1100ms

Hide
Type "q"
Sleep 300ms
