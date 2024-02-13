
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: "\n")

var totalCodeLength = 0
var totalMemoryLength = 0
var totalEncodedLength = 0

for line in lines {
    totalCodeLength += line.count
    
    let encodedLine = line.replacingOccurrences(of: "\\", with: "\\\\").replacingOccurrences(of: "\"", with: "\\\"")
    totalEncodedLength += encodedLine.count + 2 // Add 2 for the surrounding double quotes
    
    var memoryLength = 0
    var i = 1
    while i < line.count - 1 {
        if line[line.index(line.startIndex, offsetBy: i)] == "\\" {
            if line[line.index(line.startIndex, offsetBy: i + 1)] == "x" {
                i += 4
            } else {
                i += 2
            }
        }
        memoryLength += 1
        i += 1
    }
    
    totalMemoryLength += memoryLength
}

print(totalCodeLength - totalMemoryLength)
print(totalEncodedLength - totalCodeLength)
