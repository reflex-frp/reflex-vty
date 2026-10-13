# Render with: bash doc/record-example.sh prevent-quit
Output doc/examples/prevent-quit.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 220
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh prevent-quit"
Enter
Wait+Screen /Application is running/
Sleep 300ms
Show

Sleep 600ms
Type "q"
Sleep 900ms
Type "n"
Sleep 600ms
Type "q"
Sleep 900ms
Type "y"
Sleep 400ms

Hide
Sleep 300ms
