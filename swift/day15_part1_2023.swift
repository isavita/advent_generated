
import Foundation

let input = try String(contentsOfFile: "input.txt")
let steps = input.components(separatedBy: ",")

var sum = 0

for step in steps {
    var current = 0
    for char in step {
        let ascii = Int(char.asciiValue!)
        current += ascii
        current *= 17
        current %= 256
    }
    sum += current
}

print(sum)
