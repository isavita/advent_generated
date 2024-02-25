
import strutils

var frequency = 0

for line in lines("input.txt"):
    frequency += parseInt(line)

echo frequency
