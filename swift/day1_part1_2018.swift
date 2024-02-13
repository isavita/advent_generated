
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let changes = input.components(separatedBy: "\n").compactMap { Int($0) }

let result = changes.reduce(0, +)
print(result)
