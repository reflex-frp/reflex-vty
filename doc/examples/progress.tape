# Render with: bash doc/record-example.sh progress
Output doc/examples/progress.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 120
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh progress"
Enter
Wait+Screen /%/
Sleep 300ms
Show

Sleep 5s

Hide
Type "q"
Sleep 300ms
