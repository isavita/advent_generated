
import Foundation

let input = try String(contentsOfFile: "input.txt")
var score = 0
var garbageCount = 0
var ignoreNext = false
var inGarbage = false

for char in input {
    if ignoreNext {
        ignoreNext = false
    } else {
        if inGarbage {
            if char == "!" {
                ignoreNext = true
            } else if char == ">" {
                inGarbage = false
            } else {
                garbageCount += 1
            }
        } else {
            if char == "<" {
                inGarbage = true
            } else if char == "{" {
                score += 1
            } else if char == "}" {
                // Do nothing, score is calculated based on nesting
            }
        }
    }
}

print(score)
print(garbageCount)
