
import Foundation

func trimLeadingZeros(s: String) -> String {
    guard let firstNonZeroIndex = s.firstIndex(where: { $0 != "0" }) else {
        return "0"
    }
    return String(s[firstNonZeroIndex...])
}

func splitStone(s: String) -> (String, String) {
    let mid = s.count / 2
    let left = trimLeadingZeros(s: String(s.prefix(mid)))
    let right = trimLeadingZeros(s: String(s.suffix(s.count - mid)))
    return (left, right)
}

func multiplyBy2024(s: String) -> String {
    let num = s.compactMap { Int(String($0)) }
    let multiplier = [2, 0, 2, 4]
    var result = [Int](repeating: 0, count: num.count + multiplier.count)

    for i in num.indices.reversed() {
        var carry = 0
        for j in multiplier.indices.reversed() {
            let product = num[i] * multiplier[j] + result[i + j + 1] + carry
            result[i + j + 1] = product % 10
            carry = product / 10
        }
        result[i] += carry
    }

    return trimLeadingZeros(s: result.map { String($0) }.joined())
}

guard let input = try? String(contentsOfFile: "input.txt") else {
    fatalError("Error reading input.txt")
}

let stonesStr = input.split(separator: " ")
var stonesMap = [String: Int]()
for s in stonesStr {
    stonesMap[String(s), default: 0] += 1
}

for _ in 0..<75 {
    var newStonesMap = [String: Int]()
    for (stone, count) in stonesMap {
        if stone == "0" {
            newStonesMap["1", default: 0] += count
        } else if stone.count % 2 == 0 {
            let (left, right) = splitStone(s: stone)
            newStonesMap[left, default: 0] += count
            newStonesMap[right, default: 0] += count
        } else {
            let newStone = multiplyBy2024(s: stone)
            newStonesMap[newStone, default: 0] += count
        }
    }
    stonesMap = newStonesMap
}

print(stonesMap.values.reduce(0, +))
