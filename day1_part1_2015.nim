
import strutils

let input = readFile("input.txt")

var floor = 0

for i, c in input:
    if c == '(':
        floor += 1
    else:
        floor -= 1

echo floor
