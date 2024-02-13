
import Foundation

struct IPRange {
    let start: UInt32
    let end: UInt32
}

func readIPRanges(from file: String) -> [IPRange] {
    let fileURL = URL(fileURLWithPath: file)
    let content = try! String(contentsOf: fileURL)
    let lines = content.components(separatedBy: .newlines)
    
    return lines.map { line in
        let parts = line.components(separatedBy: "-")
        let start = UInt32(parts[0])!
        let end = UInt32(parts[1])!
        return IPRange(start: start, end: end)
    }
}

func findUnblockedIP(ranges: [IPRange]) -> UInt32 {
    var currentIP: UInt32 = 0
    for r in ranges {
        if r.start > currentIP {
            return currentIP
        }
        if r.end >= currentIP {
            currentIP = r.end + 1
        }
    }
    return currentIP
}

let ipRanges = readIPRanges(from: "input.txt")
let sortedRanges = ipRanges.sorted { $0.start < $1.start }
let unblockedIP = findUnblockedIP(ranges: sortedRanges)
print(unblockedIP)
