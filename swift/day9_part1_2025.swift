
import Foundation

let filePath = "input.txt"
guard var text = try? String(contentsOfFile: filePath, encoding: .utf8) else { exit(0) }

var xs = [Int]()
var ys = [Int]()
xs.reserveCapacity(10_000)
ys.reserveCapacity(10_000)

var idx = text.startIndex
let end = text.endIndex

@inline(__always) func skipSpaces() {
    while idx < end && text[idx].isWhitespace {
        idx = text.index(after: idx)
    }
}
@inline(__always) func readInt() -> Int {
    var sign = 1
    if idx < end && text[idx] == "-" {
        sign = -1
        idx = text.index(after: idx)
    }
    var value = 0
    while idx < end, let digit = text[idx].wholeNumberValue {
        value = value * 10 + digit
        idx = text.index(after: idx)
    }
    return sign * value
}

while idx < end {
    skipSpaces()
    if idx >= end { break }
    let x = readInt()
    skipSpaces()
    if idx >= end || text[idx] != "," { break }
    idx = text.index(after: idx)          // skip ','
    let y = readInt()
    xs.append(x)
    ys.append(y)
}

let n = xs.count
var best: Int64 = 0
for i in 0..<n {
    let xi = xs[i]
    let yi = ys[i]
    for j in i..<n {
        let dx = Int64(abs(xi - xs[j]) + 1)
        let dy = Int64(abs(yi - ys[j]) + 1)
        let area = dx * dy
        if area > best { best = area }
    }
}
print(best)
