
import Foundation

func floorDiv(_ a: Int, _ b: Int) -> Int {
    let q = a / b
    return (a < 0 && a % b != 0) ? q - 1 : q
}

let data = try! String(contentsOfFile: "input.txt")
let lines = data.split(separator: "\n", omittingEmptySubsequences: false)

var pos = 50
var total = 0
let size = 100

for raw in lines {
    let line = raw.trimmingCharacters(in: .whitespacesAndNewlines)
    if line.isEmpty { continue }
    let dir = line.first!
    let amount = Int(line.dropFirst())!
    if dir == "R" {
        total += (pos + amount) / size
        pos = (pos + amount) % size
    } else {
        total += floorDiv(pos - 1, size) - floorDiv(pos - amount - 1, size)
        pos = (pos - amount) % size
        if pos < 0 { pos += size }
    }
}

print("The password is: \(total)")
