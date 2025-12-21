
import Foundation

func addStrings(_ a: String, _ b: String) -> String {
    let aChars = Array(a.utf8)
    let bChars = Array(b.utf8)
    var i = aChars.count - 1
    var j = bChars.count - 1
    var carry = 0
    var res: [UInt8] = []
    while i >= 0 || j >= 0 || carry > 0 {
        let da = i >= 0 ? Int(aChars[i] - 48) : 0
        let db = j >= 0 ? Int(bChars[j] - 48) : 0
        let sum = da + db + carry
        res.append(UInt8(sum % 10 + 48))
        carry = sum / 10
        i -= 1
        j -= 1
    }
    return String(bytes: res.reversed(), encoding: .utf8)!
}

func mulStrings(_ a: String, _ b: String) -> String {
    if a == "0" || b == "0" { return "0" }
    let aDigits = a.reversed().map { Int($0.unicodeScalars.first!.value - 48) }
    let bDigits = b.reversed().map { Int($0.unicodeScalars.first!.value - 48) }
    var tmp = Array(repeating: 0, count: aDigits.count + bDigits.count)
    for i in 0..<aDigits.count {
        for j in 0..<bDigits.count {
            tmp[i + j] += aDigits[i] * bDigits[j]
        }
    }
    var carry = 0
    for k in 0..<tmp.count {
        let sum = tmp[k] + carry
        tmp[k] = sum % 10
        carry = sum / 10
    }
    var idx = tmp.count - 1
    while idx > 0 && tmp[idx] == 0 { idx -= 1 }
    let result = tmp[0...idx].reversed().map { Character(UnicodeScalar($0 + 48)!) }
    return String(result)
}

func processBlock(_ lines: [String], _ start: Int, _ end: Int, _ grandTotal: inout String) {
    var numbers: [String] = []
    var op: Character = "+"
    for c in start...end {
        var buf = ""
        for line in lines {
            if c < line.count {
                let ch = line[line.index(line.startIndex, offsetBy: c)]
                if ch.isNumber { buf.append(ch) }
                else if ch == "+" || ch == "*" { op = ch }
            }
        }
        if !buf.isEmpty { numbers.append(buf) }
    }
    if numbers.isEmpty { return }
    var blockRes = op == "*" ? "1" : "0"
    for num in numbers {
        blockRes = op == "*" ? mulStrings(blockRes, num) : addStrings(blockRes, num)
    }
    grandTotal = addStrings(grandTotal, blockRes)
}

let path = "input.txt"
guard let data = FileManager.default.contents(atPath: path),
      let content = String(data: data, encoding: .utf8) else {
    exit(1)
}
var lines = content.components(separatedBy: .newlines)
if lines.last?.isEmpty == true { lines.removeLast() }
let lineCount = lines.count
let maxWidth = lines.map { $0.count }.max() ?? 0
var isSep = Array(repeating: false, count: maxWidth)
for x in 0..<maxWidth {
    var allSpace = true
    for line in lines {
        if x < line.count, !line[line.index(line.startIndex, offsetBy: x)].isWhitespace {
            allSpace = false
            break
        }
    }
    isSep[x] = allSpace
}
var grandTotal = "0"
var inBlock = false
var start = 0
for x in 0..<maxWidth {
    if !isSep[x] {
        if !inBlock { inBlock = true; start = x }
    } else {
        if inBlock {
            processBlock(lines, start, x - 1, &grandTotal)
            inBlock = false
        }
    }
}
if inBlock { processBlock(lines, start, maxWidth - 1, &grandTotal) }
print("Grand total: \(grandTotal)")
