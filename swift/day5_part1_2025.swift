
import Foundation

let input = try! String(contentsOfFile: "input.txt")
var ranges = [(Int, Int)]()
var fresh = 0
var parsing = true

for raw in input.split(separator: "\n", omittingEmptySubsequences: false) {
    let line = raw.trimmingCharacters(in: .whitespacesAndNewlines)
    if line.isEmpty {
        if parsing { parsing = false }
        continue
    }
    if parsing {
        let parts = line.split(separator: "-")
        if parts.count == 2,
           let lo = Int(parts[0]),
           let hi = Int(parts[1]) {
            ranges.append((lo, hi))
        }
    } else {
        if let id = Int(line) {
            for r in ranges where id >= r.0 && id <= r.1 {
                fresh += 1
                break
            }
        }
    }
}

print("Number of fresh ingredients: \(fresh)")
