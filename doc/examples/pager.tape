# Render with: bash doc/record-example.sh pager
Output doc/examples/pager.gif
Set Shell "bash"
Set FontFamily "DejaVu Sans Mono"
Set FontSize 20
Set Width 720
Set Height 320
Set Padding 16
Set Theme "Dracula"
Set WaitTimeout 180s

Hide
Type "bash doc/run-examples.sh pager"
Enter
Wait+Screen /scroll/
Sleep 300ms
Show

Sleep 700ms
Down@90ms 12
Sleep 1000ms

Hide
Type "q"
Sleep 300ms
