import Foundation

func calculateMaxJoltage(_ bank: String) -> Int {
    let chars = Array(bank)
    for d1 in stride(from: 9, through: 0, by: -1) {
        let d1Char: Character = Character(UnicodeScalar(48 + d1)!)
        if let i = chars.firstIndex(of: d1Char), i < chars.count - 1 {
            var maxD2 = -1
            for j in (i + 1)..<chars.count {
                let c = chars[j]
                if let v = c.wholeNumberValue, v > maxD2 {
                    maxD2 = v
                    if maxD2 == 9 { break }
                }
            }
            if maxD2 != -1 { return d1 * 10 + maxD2 }
        }
    }
    return 0
}

do {
    let content = try String(contentsOfFile: "input.txt", encoding: .utf8)
    var total = 0
    for raw in content.split(separator: "\n", omittingEmptySubsequences: false) {
        let line = raw.trimmingCharacters(in: .whitespacesAndNewlines)
        if line.isEmpty { continue }
        total += calculateMaxJoltage(line)
    }
    print("Total output joltage: \(total)")
} catch {
    fatalError("\(error)")
}