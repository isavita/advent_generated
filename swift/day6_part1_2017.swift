
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
var banks = input.components(separatedBy: .whitespacesAndNewlines).compactMap { Int($0) }

var configurations: [[Int]] = []
var cycles = 0

while !configurations.contains(banks) {
    configurations.append(banks)
    
    var maxBlocks = banks.max() ?? 0
    var index = banks.firstIndex(of: maxBlocks) ?? 0
    banks[index] = 0
    
    while maxBlocks > 0 {
        index = (index + 1) % banks.count
        banks[index] += 1
        maxBlocks -= 1
    }
    
    cycles += 1
}

print(cycles)
