# Render with: bash doc/record-example.sh spinner
Output doc/examples/spinner.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 120
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh spinner"
Enter
Wait+Screen /Spinning/
Sleep 300ms
Show

Sleep 3500ms

Hide
Type "q"
Sleep 300ms
