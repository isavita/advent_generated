
import Foundation

func readAll(from path: String) -> String {
    return try! String(contentsOfFile: path)
}

func compare(_ a: Any, _ b: Any) -> Int {
    if let aNum = a as? Double, let bNum = b as? Double {
        return aNum < bNum ? -1 : (aNum > bNum ? 1 : 0)
    } else if let aNum = a as? Double {
        return compare([aNum], b)
    } else if let bNum = b as? Double {
        return compare(a, [bNum])
    } else {
        let aa = a as! [Any]
        let bb = b as! [Any]
        for i in 0..<min(aa.count, bb.count) {
            let result = compare(aa[i], bb[i])
            if result != 0 { return result }
        }
        return aa.count < bb.count ? -1 : (aa.count > bb.count ? 1 : 0)
    }
}

let input = readAll(from: "input.txt")
var packets: [Any] = []

for pair in input.split(separator: "\n\n") {
    let sp = pair.split(separator: "\n").map { String($0) }
    let first = try! JSONSerialization.jsonObject(with: Data(sp[0].utf8), options: [])
    let second = try! JSONSerialization.jsonObject(with: Data(sp[1].utf8), options: [])
    packets.append(first)
    packets.append(second)
}

let divider1 = try! JSONSerialization.jsonObject(with: Data("[[2]]".utf8), options: [])
let divider2 = try! JSONSerialization.jsonObject(with: Data("[[6]]".utf8), options: [])
packets.append(divider1)
packets.append(divider2)

packets.sort { compare($0, $1) < 0 }

let divider1Pos = packets.firstIndex(where: { compare($0, divider1) >= 0 })! + 1
let divider2Pos = packets.firstIndex(where: { compare($0, divider2) >= 0 })! + 1

print(divider1Pos * divider2Pos)
