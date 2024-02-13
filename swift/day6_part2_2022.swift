
import Foundation

func readAll(_ path: String) -> String {
    return try! String(contentsOfFile: path).trimmingCharacters(in: .whitespacesAndNewlines)
}

func firstNUnique(_ s: String, _ n: Int) -> Int {
    let s = Array(s)
    for i in n..<s.count {
        let sub = Array(s[i-n..<i])
        if Set(sub).count == sub.count {
            return i
        }
    }
    return -1
}

let path = "input.txt"
let s = readAll(path)
print(firstNUnique(s, 14))
