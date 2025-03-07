
import Foundation

// MARK: - Snailfish Number Representation

indirect enum SnailfishNumber {
    case regular(Int)
    case pair(SnailfishNumber, SnailfishNumber)

    // MARK: - Parsing from String

    static func parse(from string: String) -> SnailfishNumber {
        var index = string.startIndex
        return parseNumber(from: string, index: &index)
    }

    private static func parseNumber(from string: String, index: inout String.Index) -> SnailfishNumber {
        if string[index] == "[" {
            index = string.index(after: index) // Skip "["
            let left = parseNumber(from: string, index: &index)
            index = string.index(after: index) // Skip ","
            let right = parseNumber(from: string, index: &index)
            index = string.index(after: index) // Skip "]"
            return .pair(left, right)
        } else {
            let start = index
            while string[index].isNumber {
                index = string.index(after: index)
            }
            let num = Int(string[start..<index])!
            return .regular(num)
        }
    }
    
    // MARK: - Magnitude Calculation
    var magnitude: Int {
        switch self {
        case .regular(let value):
            return value
        case .pair(let left, let right):
            return 3 * left.magnitude + 2 * right.magnitude
        }
    }

    // MARK: - String Representation (for debugging)

    var description: String {
        switch self {
        case .regular(let value):
            return "\(value)"
        case .pair(let left, let right):
            return "[\(left.description),\(right.description)]"
        }
    }
}

// MARK: - Reduction Operations (Explode and Split)

extension SnailfishNumber {
    
    mutating func reduce() {
        while true {
            if let _ = explode() {
                continue
            }
            if !split() {
                break
            }
        }
    }

    private mutating func explode() -> (Bool, Int, Int)? {
        return explode(depth: 0)
    }
    
    private mutating func explode(depth: Int) -> (Bool, Int, Int)? {
          switch self {
          case .regular:
              return nil

          case .pair(var left, var right):
              if depth >= 4 {
                  // Explode this pair
                  if case let .regular(leftValue) = left, case let .regular(rightValue) = right {
                      self = .regular(0)
                      return (true, leftValue, rightValue)
                  } else {
                    preconditionFailure("Exploding pair should always contain two regular numbers.")
                  }
              }

              // Try to explode the left side
              if let (exploded, leftAdd, rightAdd) = left.explode(depth: depth + 1) {
                  if exploded {
                      if rightAdd > 0 {
                          right.addToLeftmost(rightAdd)
                      }
                      self = .pair(left, right) // Rebuild, as left has changed
                      return (true, leftAdd, 0) // Pass leftAdd up, rightAdd was handled
                  }
              }

              // Try to explode the right side
              if let (exploded, leftAdd, rightAdd) = right.explode(depth: depth + 1) {
                if exploded {
                    if leftAdd > 0 {
                        left.addToRightmost(leftAdd)
                    }
                    self = .pair(left, right) // Rebuild, as right has changed
                    return (true, 0, rightAdd) // Pass rightAdd up, leftAdd was handled
                }
              }

              return nil // Nothing exploded
          }
      }
    
    private mutating func addToLeftmost(_ value: Int) {
        switch self {
        case .regular(let num):
            self = .regular(num + value)
        case .pair(var left, let right):
            left.addToLeftmost(value)
            self = .pair(left, right) // Rebuild as left might have changed
        }
    }

    private mutating func addToRightmost(_ value: Int) {
        switch self {
        case .regular(let num):
            self = .regular(num + value)
        case .pair(let left, var right):
            right.addToRightmost(value)
            self = .pair(left, right) // Rebuild as right might have changed

        }
    }
    private mutating func split() -> Bool {
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
                self = .pair(left, right)  //rebuild
                return true
            }
            if right.split() {
                self = .pair(left, right)  //rebuild
                return true
            }
            return false
        }
    }
}

// MARK: - Addition Operator

func + (left: SnailfishNumber, right: SnailfishNumber) -> SnailfishNumber {
    var result = SnailfishNumber.pair(left, right)
    result.reduce()
    return result
}

// MARK: - Main Function
func main() {
    do {
        // Read input from file
        let input = try String(contentsOfFile: "input.txt")
        let lines = input.split(separator: "\n").map { String($0) }

        // Parse snailfish numbers
        let numbers = lines.map { SnailfishNumber.parse(from: $0) }

        // Add all numbers together
        guard var sum = numbers.first else {
            print("Input file is empty.")
            return
        }
        
        for number in numbers.dropFirst() {
            sum = sum + number
        }
        // Print the magnitude of the final sum
        print(sum.magnitude)

    } catch {
        print("Error reading file: \(error)")
    }
}

// MARK: - Entry Point

main()
