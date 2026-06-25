# Render with: bash doc/record-example.sh textinput
Output doc/examples/textinput.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 180
Set Padding 16
Set Theme "Dracula"
Set TypingSpeed 90ms
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh textinput"
Enter
Wait+Screen /name/
Sleep 400ms
Show

Sleep 600ms
Type "Ada Lovelace"
Sleep 1800ms

Hide
Ctrl+C
Sleep 300ms
