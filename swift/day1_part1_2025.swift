
import Foundation

let content = try String(contentsOfFile: "input.txt", encoding: .utf8)
var pos = 50
var zeroCount = 0

for raw in content.split(separator: "\n") {
    let line = raw.trimmingCharacters(in: .whitespacesAndNewlines)
    if line.isEmpty { continue }
    let dir = line.first!
    let amount = Int(line.dropFirst())!
    if dir == "R" {
        pos = (pos + amount) % 100
    } else {
        pos = (pos - amount) % 100
        if pos < 0 { pos += 100 }
    }
    if pos == 0 { zeroCount += 1 }
}

print("The password is: \(zeroCount)")
