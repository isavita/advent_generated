
import Foundation

// ---------- Helpers ----------
@inline(__always) func pow10(_ e: Int) -> UInt64 {
    var v: UInt64 = 1
    for _ in 0..<e { v *= 10 }
    return v
}

// ---------- Read & parse ----------
let data = try String(contentsOfFile: "input.txt", encoding: .utf8)
    .replacingOccurrences(of: "\n", with: "")
    .replacingOccurrences(of: "\r", with: "")
    .trimmingCharacters(in: .whitespaces)

let parts = data.split(separator: ",")
var ranges: [(UInt64, UInt64)] = []

for p in parts where !p.isEmpty {
    let b = p.split(separator: "-")
    guard b.count == 2,
          let lo = UInt64(b[0]),
          let hi = UInt64(b[1]) else { fatalError("invalid range") }
    ranges.append((lo, hi))
}

// ---------- Find repeated‑twice IDs ----------
var found = Set<UInt64>()

for (lo, hi) in ranges {
    // k = length of the seed, ID length = 2·k
    for k in 1...9 {                     // 2·k ≤ 18 ⇒ fits in UInt64
        let multiplier = pow10(k) + 1               // 10^k + 1
        let minSeed = (k == 1) ? 1 : pow10(k - 1)    // 10^(k‑1)
        let maxSeed = pow10(k) - 1                   // 10^k – 1

        // ceil(lo / multiplier)
        let sMin = (lo + multiplier - 1) / multiplier
        // floor(hi / multiplier)
        let sMax = hi / multiplier

        // intersect with valid seed range
        let start = max(sMin, minSeed)
        let end   = min(sMax, maxSeed)
        if start > end { continue }

        var seed = start
        while seed <= end {
            let id = seed * multiplier          // seed·(10^k+1)
            found.insert(id)
            seed += 1
        }
    }
}

// ---------- Sum (using Decimal for safety) ----------
var total = Decimal(0)
for id in found {
    total += Decimal(string: String(id))!
}

// ---------- Output ----------
print("Sum of invalid IDs: \(total)")
