
import Foundation

let input = try String(contentsOfFile: "input.txt")
var score = 0
var level = 0
var inGarbage = false
var ignoreNext = false

for char in input {
    if ignoreNext {
        ignoreNext = false
    } else {
        if inGarbage {
            if char == ">" {
                inGarbage = false
            } else if char == "!" {
                ignoreNext = true
            }
        } else {
            if char == "<" {
                inGarbage = true
            } else if char == "{" {
                level += 1
                score += level
            } else if char == "}" {
                level -= 1
            }
        }
    }
}

print(score)
