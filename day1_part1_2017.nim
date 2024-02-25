
import strutils

let input = readFile("input.txt").strip()

var sum = 0
for i in 0..<input.len:
    if input[i] == input[(i + 1) mod input.len]:
        sum += parseInt($input[i..i])

echo sum
