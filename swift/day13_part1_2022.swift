
import Foundation

func readAll(_ path: String) -> String {
    do {
        return try String(contentsOfFile: path)
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

func compare(_ a: Any, _ b: Any) -> Int {
    if let anum = a as? Double, let bnum = b as? Double {
        return sign(Int(anum) - Int(bnum))
    } else if let aa = a as? [Any], let bb = b as? [Any] {
        for i in 0..<min(aa.count, bb.count) {
            if let c = compare(aa[i], bb[i]) as Int?, c != 0 {
                return c
            }
        }
        return sign(aa.count - bb.count)
    } else if let _ = a as? Double {
        return compare([a], b)
    } else if let _ = b as? Double {
        return compare(a, [b])
    }
    return 0
}

func sign(_ n: Int) -> Int {
    if n == 0 {
        return 0
    } else if n < 0 {
        return -1
    } else {
        return 1
    }
}

let path = "input.txt"
let s = readAll(path)
var packets: [Any] = []
var sum = 0

for (i, pair) in s.components(separatedBy: "\n\n").enumerated() {
    let sp = pair.components(separatedBy: "\n")
    if let first = try? JSONSerialization.jsonObject(with: Data(sp[0].utf8), options: []) as Any,
       let second = try? JSONSerialization.jsonObject(with: Data(sp[1].utf8), options: []) as Any {
        packets.append(first)
        packets.append(second)
        if compare(first, second) == -1 {
            sum += i + 1
        }
    }
}

print(sum)
