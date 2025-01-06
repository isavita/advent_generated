
import Foundation

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Error reading input file.")
        return
    }

    let pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)"
    guard let regex = try? NSRegularExpression(pattern: pattern) else {
        print("Error creating regular expression.")
        return
    }

    let matches = regex.matches(in: input, range: NSRange(input.startIndex..., in: input))
    let sum = matches.reduce(0) { sum, match in
        guard match.numberOfRanges == 3,
              let firstRange = Range(match.range(at: 1), in: input),
              let secondRange = Range(match.range(at: 2), in: input),
              let firstNum = Int(input[firstRange]),
              let secondNum = Int(input[secondRange]) else {
            return sum
        }
        return sum + (firstNum * secondNum)
    }

    print(sum)
}

solve()
