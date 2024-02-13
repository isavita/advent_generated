
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var codeCount = 0
var memoryCount = 0

for line in lines {
    codeCount += line.count

    var i = 1
    while i < line.count - 1 {
        if line[line.index(line.startIndex, offsetBy: i)] == "\\" {
            if line[line.index(line.startIndex, offsetBy: i+1)] == "\\" || line[line.index(line.startIndex, offsetBy: i+1)] == "\"" {
                memoryCount += 1
                i += 2
            } else if line[line.index(line.startIndex, offsetBy: i+1)] == "x" {
                memoryCount += 1
                i += 4
            }
        } else {
            memoryCount += 1
            i += 1
        }
    }
}

print(codeCount - memoryCount)
