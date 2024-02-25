
import os
import strutils

var file = open("input.txt")
let steps = file.readLine().parseInt()

var currentPos = 0
var valueAfterZero = 0

for i in 1..50000000:
    currentPos = (currentPos + steps) mod i
    if currentPos == 0:
        valueAfterZero = i
    currentPos += 1

echo valueAfterZero
