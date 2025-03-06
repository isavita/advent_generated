
import Foundation

indirect enum SnailfishNumber {
    case regular(Int)
    case pair(SnailfishNumber, SnailfishNumber)

    func add(_ other: SnailfishNumber) -> SnailfishNumber {
        var sum = SnailfishNumber.pair(self, other)
        sum.reduce()
        return sum
    }

    mutating func reduce() {
        while true {
            if self.explode() {
                continue
            }
            if self.split() {
                continue
            }
            break
        }
    }

    mutating func explode() -> Bool {
        return explode(depth: 0).didExplode
    }

    mutating func explode(depth: Int) -> (didExplode: Bool, addToLeft: Int?, addToRight: Int?) {
        switch self {
        case .regular:
            return (false, nil, nil)
        case .pair(var left, var right):
            if depth >= 4 {
                let leftValue = left.regularValue!
                let rightValue = right.regularValue!
                self = .regular(0)
                return (true, leftValue, rightValue)
            }

            let leftResult = left.explode(depth: depth + 1)
            if leftResult.didExplode {
                if let rightAdd = leftResult.addToRight {
                    right.addFromLeft(rightAdd)
                }
                self = .pair(left, right)
                return (true, leftResult.addToLeft, nil)
            }

            let rightResult = right.explode(depth: depth + 1)
            if rightResult.didExplode {
                if let leftAdd = rightResult.addToLeft {
                    left.addFromRight(leftAdd)
                }
                self = .pair(left, right)
                return (true, nil, rightResult.addToRight)
            }

            return (false, nil, nil)
        }
    }
    
    mutating func addFromLeft(_ value: Int) {
        switch self {
        case .regular(let num):
            self = .regular(num + value)
        case .pair(var left, let right):
            left.addFromLeft(value)
            self = .pair(left, right)
        }
    }

    mutating func addFromRight(_ value: Int) {
        switch self {
        case .regular(let num):
            self = .regular(num + value)
        case .pair(let left, var right):
            right.addFromRight(value)
            self = .pair(left, right)
        }
    }

    var regularValue: Int? {
        switch self {
            case .regular(let value) : return value
            case .pair(_, _): return nil
        }
    }


    mutating func split() -> Bool {
        switch self {
        case .regular(let value):
            if value >= 10 {
                let leftValue = value / 2
                let rightValue = value - leftValue
                self = .pair(.regular(leftValue), .regular(rightValue))
                return true
            }
            return false
        case .pair(var left, var right):
            if left.split() {
                self = .pair(left, right)
                return true
            }
            if right.split() {
                self = .pair(left, right)
                return true
            }
            return false
        }
    }

    var magnitude: Int {
        switch self {
        case .regular(let value):
            return value
        case .pair(let left, let right):
            return 3 * left.magnitude + 2 * right.magnitude
        }
    }
    
    func copy() -> SnailfishNumber {
        switch self {
            case .regular(let value):
                return .regular(value)
            case .pair(let left, let right):
                return .pair(left.copy(), right.copy())
        }
    }
}

extension SnailfishNumber: CustomStringConvertible {
    var description: String {
        switch self {
        case .regular(let value):
            return "\(value)"
        case .pair(let left, let right):
            return "[\(left),\(right)]"
        }
    }
}


func parseSnailfishNumber(_ string: String) -> SnailfishNumber {
    var index = string.startIndex
    return parseSnailfishNumber(string, &index)
}

func parseSnailfishNumber(_ string: String, _ index: inout String.Index) -> SnailfishNumber {
    if string[index] == "[" {
        index = string.index(after: index) // Skip '['
        let left = parseSnailfishNumber(string, &index)
        index = string.index(after: index) // Skip ','
        let right = parseSnailfishNumber(string, &index)
        index = string.index(after: index) // Skip ']'
        return .pair(left, right)
    } else {
        let start = index
        while string[index].isNumber {
            index = string.index(after: index)
        }
        let numStr = string[start..<index]
        return .regular(Int(numStr)!)
    }
}


func part1(_ numbers: [SnailfishNumber]) -> Int {
    var sum = numbers[0]
    for i in 1..<numbers.count {
        sum = sum.add(numbers[i])
    }
    return sum.magnitude
}

func part2(_ numbers: [SnailfishNumber]) -> Int {
    var maxMagnitude = 0
    for i in 0..<numbers.count {
        for j in 0..<numbers.count {
            if i != j {
                let magnitude = numbers[i].copy().add(numbers[j].copy()).magnitude
                maxMagnitude = max(maxMagnitude, magnitude)
            }
        }
    }
    return maxMagnitude
}
func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
        let snailfishNumbers = lines.map { parseSnailfishNumber($0) }

        print("Part 1: \(part1(snailfishNumbers))")
        print("Part 2: \(part2(snailfishNumbers))")
        
    } catch {
        print("Error reading file: \(error)")
    }
}

main()
