
import Foundation

func add(_ a: String, _ b: String) -> String {
    let aDigits = a.reversed().compactMap { $0.wholeNumberValue }
    let bDigits = b.reversed().compactMap { $0.wholeNumberValue }
    var i = 0, carry = 0
    var result: [Int] = []
    while i < aDigits.count || i < bDigits.count || carry > 0 {
        let da = i < aDigits.count ? aDigits[i] : 0
        let db = i < bDigits.count ? bDigits[i] : 0
        let sum = da + db + carry
        result.append(sum % 10)
        carry = sum / 10
        i += 1
    }
    while result.count > 1 && result.last == 0 { result.removeLast() }
    return result.reversed().map(String.init).joined()
}

func mul(_ a: String, _ b: String) -> String {
    if a == "0" || b == "0" { return "0" }
    let aDigits = a.reversed().compactMap { $0.wholeNumberValue }
    let bDigits = b.reversed().compactMap { $0.wholeNumberValue }
    var prod = Array(repeating: 0, count: aDigits.count + bDigits.count)

    for i in 0..<aDigits.count {
        for j in 0..<bDigits.count {
            prod[i + j] += aDigits[i] * bDigits[j]
        }
    }
    var carry = 0
    for k in 0..<prod.count {
        let total = prod[k] + carry
        prod[k] = total % 10
        carry = total / 10
    }
    while prod.last == 0 { prod.removeLast() }
    return prod.reversed().map(String.init).joined()
}

func isSeparator(_ col: Int, _ lines: [String]) -> Bool {
    for line in lines {
        if col < line.count && !line[line.index(line.startIndex, offsetBy: col)].isWhitespace {
            return false
        }
    }
    return true
}

func processBlock(sc: Int, ec: Int, lines: [String], grand: inout String) {
    var numbers: [String] = []
    var op = 0          // 0‑none, 1‑+, 2‑*
    for line in lines {
        let end = min(ec + 1, line.count)
        guard sc < end else { continue }
        let startIdx = line.index(line.startIndex, offsetBy: sc)
        let endIdx   = line.index(line.startIndex, offsetBy: end)
        var seg = String(line[startIdx..<endIdx])
        seg = seg.trimmingCharacters(in: .whitespaces)
        guard !seg.isEmpty else { continue }
        if seg == "+" { op = 1 }
        else if seg == "*" { op = 2 }
        else { numbers.append(seg) }
    }
    guard !numbers.isEmpty else { return }

    var blockResult = "0"
    if op == 1 {
        for n in numbers { blockResult = add(blockResult, n) }
    } else if op == 2 {
        blockResult = "1"
        for n in numbers { blockResult = mul(blockResult, n) }
    } else if numbers.count == 1 {
        blockResult = numbers[0]
    }
    grand = add(grand, blockResult)
}

/* ---------- main ---------- */

let path = "input.txt"
guard let data = try? String(contentsOfFile: path, encoding: .utf8) else {
    exit(1)
}
var lines = data.components(separatedBy: .newlines)
if lines.last == "" { lines.removeLast() }

let maxWidth = lines.map { $0.count }.max() ?? 0
var grand = "0"
var inBlock = false
var startCol = 0

for col in 0..<maxWidth {
    if !isSeparator(col, lines) {
        if !inBlock { inBlock = true; startCol = col }
    } else {
        if inBlock {
            processBlock(sc: startCol, ec: col - 1, lines: lines, grand: &grand)
            inBlock = false
        }
    }
}
if inBlock {
    processBlock(sc: startCol, ec: maxWidth - 1, lines: lines, grand: &grand)
}

print("Grand total: \(grand)")
