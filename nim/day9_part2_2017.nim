

import strutils

var file = open("input.txt")
let input = file.readAll().strip()

var score = 0
var depth = 0
var inGarbage = false
var cancelNext = false
var garbageCount = 0

for ch in input:
    if cancelNext:
        cancelNext = false
        continue

    if inGarbage:
        if ch == '!':
            cancelNext = true
        elif ch == '>':
            inGarbage = false
        else:
            inc garbageCount
    else:
        case ch
        of '{':
            inc depth
        of '}':
            inc score, -depth
        of '<':
            inGarbage = true
        else:
            # Add case for any other characters that are not covered in the existing cases
            discard

echo garbageCount

