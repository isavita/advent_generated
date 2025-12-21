import Foundation

func readLines() -> [String] {
    let path = "input.txt"
    guard let content = try? String(contentsOfFile: path) else { fatalError("Cannot open file") }
    return content.split(separator: "\n", omittingEmptySubsequences: false).map { String($0) }
}

var ranges = [(min: Int, max: Int)]()
for line in readLines() {
    let trimmed = line.trimmingCharacters(in: .whitespacesAndNewlines)
    if trimmed.isEmpty { break }
    let parts = trimmed.split(separator: "-")
    guard parts.count == 2,
          let a = Int(parts[0]),
          let b = Int(parts[1]) else { fatalError("Invalid range: \(trimmed)") }
    let lo = min(a, b), hi = max(a, b)
    ranges.append((min: lo, max: hi))
}

guard !ranges.isEmpty else { print("Total fresh IDs: 0"); exit(0) }

ranges.sort { $0.min < $1.min }

var total = 0
var curMin = ranges[0].min
var curMax = ranges[0].max

for r in ranges.dropFirst() {
    if r.min <= curMax {
        if r.max > curMax { curMax = r.max }
    } else {
        total += curMax - curMin + 1
        curMin = r.min
        curMax = r.max
    }
}
total += curMax - curMin + 1

print("Total fresh IDs: \(total)")