
import Foundation

// ---------- Big integer handling (base 1_000_000_000) ----------
typealias BigChunk = UInt32          // fits 0 … 999_999_999
let BASE: UInt64 = 1_000_000_000

// sum is stored little‑endian (least‑significant chunk first)
func addBigInt(_ sum: inout [BigChunk], _ value: String) {
    // split value into base‑BASE chunks
    var chunks: [BigChunk] = []
    var i = value.endIndex
    while i > value.startIndex {
        let start = value.index(i, offsetBy: -9, limitedBy: value.startIndex) ?? value.startIndex
        let part = String(value[start..<i])
        chunks.append(BigChunk(part)!)
        i = start
    }

    // add chunks to sum
    var carry: UInt64 = 0
    let maxLen = max(sum.count, chunks.count)
    if sum.count < maxLen { sum.append(contentsOf: repeatElement(0, count: maxLen - sum.count)) }
    for idx in 0..<maxLen {
        let a = UInt64(sum[idx])
        let b = idx < chunks.count ? UInt64(chunks[idx]) : 0
        let total = a + b + carry
        sum[idx] = BigChunk(total % BASE)
        carry = total / BASE
    }
    if carry > 0 { sum.append(BigChunk(carry)) }
}

// convert stored big‑int to decimal string
func bigIntToString(_ sum: [BigChunk]) -> String {
    guard !sum.isEmpty else { return "0" }
    var result = String(sum.last!)
    for chunk in sum.dropLast().reversed() {
        let part = String(chunk)
        result += String(repeating: "0", count: 9 - part.count) + part
    }
    return result
}

// ---------- Max 12‑digit subsequence ----------
func maxSubsequence(_ s: String, length k: Int) -> String {
    let bytes = Array(s.utf8)
    var stack = [UInt8]()
    var toRemove = bytes.count - k

    for b in bytes {
        while toRemove > 0 && !stack.isEmpty && stack.last! < b {
            stack.removeLast()
            toRemove -= 1
        }
        stack.append(b)
    }
    // keep only first k digits
    return String(bytes: stack.prefix(k), encoding: .utf8)!
}

// ---------- Main ----------
func main() {
    let path = "input.txt"
    guard let data = try? Data(contentsOf: URL(fileURLWithPath: path)),
          let content = String(data: data, encoding: .utf8) else {
        fatalError("Cannot read input.txt")
    }

    var total: [BigChunk] = []          // big‑int accumulator
    let target = 12

    for line in content.split(separator: "\n", omittingEmptySubsequences: false) {
        let trimmed = line.trimmingCharacters(in: .whitespacesAndNewlines)
        guard trimmed.count >= target else { continue }

        let best = maxSubsequence(trimmed, length: target)
        addBigInt(&total, best)
    }

    print("Total output joltage: \(bigIntToString(total))")
}

main()
