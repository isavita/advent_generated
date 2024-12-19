
import Foundation

func solve() -> Int {
    guard let data = try? String(contentsOfFile: "input.txt") else { return 0 }
    var stones = data.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: .whitespaces)

    for _ in 0..<25 {
        var next = [String]()
        next.reserveCapacity(stones.count * 2)
        for s in stones {
            if s == "0" {
                next.append("1")
            } else if s.count % 2 == 0 {
                let mid = s.count / 2
                let left = trimLeadingZeros(String(s.prefix(mid)))
                let right = trimLeadingZeros(String(s.suffix(mid)))
                next.append(left.isEmpty ? "0" : left)
                next.append(right.isEmpty ? "0" : right)
            } else {
                if let n = Int(s) {
                    next.append(String(n * 2024))
                }
            }
        }
        stones = next
    }
    return stones.count
}

func trimLeadingZeros(_ s: String) -> String {
    var s = s
    while s.count > 1 && s.first == "0" {
        s.removeFirst()
    }
    return s
}

print(solve())
