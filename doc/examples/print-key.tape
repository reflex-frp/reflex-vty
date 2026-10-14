# Render with: bash doc/record-example.sh print-key
Output doc/examples/print-key.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 220
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh print-key"
Enter
Wait+Screen /Keyboard events/
Sleep 300ms
Show

Type "a"
Sleep 700ms
Up
Sleep 700ms
Tab
Sleep 700ms
Ctrl+A
Sleep 700ms

Hide
Ctrl+C
Sleep 300ms
