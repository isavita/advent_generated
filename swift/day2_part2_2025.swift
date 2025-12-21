import Foundation

func isInvalid(_ x: UInt64) -> Bool {
    let s = String(x)
    let n = s.count
    guard n > 1 else { return false }
    let chars = Array(s)
    for p in 1...n/2 where n % p == 0 {
        let k = n / p
        guard k >= 2 else { continue }
        var ok = true
        for i in p..<n where ok {
            if chars[i] != chars[i % p] { ok = false }
        }
        if ok { return true }
    }
    return false
}

let data = try Data(contentsOf: URL(fileURLWithPath: "input.txt"))
let buf = String(decoding: data, as: UTF8.self)
var i = buf.startIndex
var sum: UInt64 = 0

while i < buf.endIndex {
    while i < buf.endIndex, " \n\r\t," .contains(buf[i]) { i = buf.index(after: i) }
    guard i < buf.endIndex else { break }
    var aStr = ""
    while i < buf.endIndex, let d = buf[i].wholeNumberValue { aStr.append(buf[i]); i = buf.index(after: i) }
    guard !aStr.isEmpty, i < buf.endIndex, buf[i] == "-" else { break }
    i = buf.index(after: i)
    var bStr = ""
    while i < buf.endIndex, let d = buf[i].wholeNumberValue { bStr.append(buf[i]); i = buf.index(after: i) }
    guard let aVal = UInt64(aStr), let bVal = UInt64(bStr) else { break }
    var a = aVal, b = bVal
    if a > b { swap(&a, &b) }
    var x = a
    while true {
        if isInvalid(x) { sum = sum &+ x }
        if x == b || x == UInt64.max { break }
        x &+= 1
    }
}

print(sum)