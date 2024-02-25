
import os
import strutils

var file = open("input.txt")
let input = file.readAll().strip()
let halfway = len(input) div 2
var sum = 0

for i in 0..<len(input):
    var next = (i + halfway) mod len(input)
    if input[i] == input[next]:
        sum += int(input[i].ord - '0'.ord)

echo sum
