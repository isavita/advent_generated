
import Foundation

// ---------- simple arbitrary‑precision integer (addition only) ----------
struct BigInt: CustomStringConvertible {
    private static let base = 1_000_000_000
    private var d: [Int]          // little‑endian digits

    init(_ v: Int = 0) {
        d = v == 0 ? [] : [v]
        normalize()
    }

    private mutating func normalize() {
        var i = 0
        var carry = 0
        while i < d.count {
            let val = d[i] + carry
            if val >= BigInt.base {
                d[i] = val - BigInt.base
                carry = 1
            } else {
                d[i] = val
                carry = 0
            }
            i += 1
        }
        if carry > 0 { d.append(carry) }
        while let last = d.last, last == 0 { d.removeLast() }
    }

    mutating func add(_ other: BigInt) {
        let maxLen = max(d.count, other.d.count)
        if d.count < maxLen { d.append(contentsOf: repeatElement(0, count: maxLen - d.count)) }
        var carry = 0
        for i in 0..<maxLen {
            let sum = d[i] + (i < other.d.count ? other.d[i] : 0) + carry
            d[i] = sum % BigInt.base
            carry = sum / BigInt.base
        }
        if carry > 0 { d.append(carry) }
    }

    var description: String {
        if d.isEmpty { return "0" }
        var s = String(d.last!)
        for digit in d.dropLast().reversed() {
            let part = String(digit)
            s += String(repeating: "0", count: 9 - part.count) + part
        }
        return s
    }
}

// ---------- helper to add a BigInt into a dictionary ----------
func add(_ dict: inout [Int: BigInt], key: Int, value: BigInt) {
    if var existing = dict[key] {
        existing.add(value)
        dict[key] = existing
    } else {
        dict[key] = value
    }
}

// ---------- read input ----------
let raw = try String(contentsOfFile: "input.txt", encoding: .utf8)
let allLines = raw.split(separator: "\n", omittingEmptySubsequences: false).map { String($0) }
let grid = allLines.filter { !$0.isEmpty }

guard !grid.isEmpty else { print("0"); exit(0) }

let height = grid.count
let width = grid[0].count

// ---------- locate start ----------
var startX = -1, startY = -1, found = false
for (y, row) in grid.enumerated() where !found {
    for (x, ch) in row.enumerated() where ch == "S" {
        startX = x; startY = y; found = true; break
    }
}
guard found else { fatalError("Start point 'S' not found") }

// ---------- simulation ----------
var counts: [Int: BigInt] = [startX: BigInt(1)]

for y in startY..<height {
    var next: [Int: BigInt] = [:]
    for (x, cnt) in counts {
        var splitter = false
        if x >= 0 && x < width {
            let rowArr = Array(grid[y])
            if rowArr[x] == "^" { splitter = true }
        }
        if splitter {
            add(&next, key: x - 1, value: cnt)
            add(&next, key: x + 1, value: cnt)
        } else {
            add(&next, key: x, value: cnt)
        }
    }
    counts = next
}

// ---------- sum and output ----------
var total = BigInt(0)
for v in counts.values { total.add(v) }
print(total)
