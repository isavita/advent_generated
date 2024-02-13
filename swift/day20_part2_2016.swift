
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var blockedRanges: [(Int, Int)] = []

for line in lines {
    let components = line.components(separatedBy: "-")
    let start = Int(components[0])!
    let end = Int(components[1])!
    blockedRanges.append((start, end))
}

blockedRanges.sort { $0.0 < $1.0 }

var allowedIPs: [Int] = []

var currentIP = 0

for range in blockedRanges {
    if currentIP < range.0 {
        allowedIPs.append(contentsOf: currentIP..<range.0)
    }
    currentIP = max(currentIP, range.1 + 1)
}

let lowestAllowedIP = allowedIPs.first!
let numberOfAllowedIPs = allowedIPs.count

print(lowestAllowedIP)
print(numberOfAllowedIPs)
