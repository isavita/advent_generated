
import Foundation

// Define a struct to encapsulate the calibration logic.  This promotes reusability and clarity.
struct Calibration {

    let line: String

    init(line: String) {
        self.line = line
    }
    
    // Function to extract digits, handling both numeric and spelled-out digits.
    private func extractDigits(allowSpelledOut: Bool) -> [Int] {
        var digits: [Int] = []
        let digitStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

        for (index, char) in line.enumerated() {
            if let digit = Int(String(char)) {
                digits.append(digit)
            } else if allowSpelledOut {
                for (digitValue, digitString) in digitStrings.enumerated() {
                    // Check if the spelled-out digit starts at the current index.
                    // Efficiently checks using hasPrefix on a substring.
                    if line.dropFirst(index).hasPrefix(digitString) {
                        digits.append(digitValue + 1) // +1 because digitValue is 0-indexed
                        break // Only consider the first matching spelled-out digit
                    }
                }
            }
        }
        return digits
    }

    // Calculate calibration value for Part 1 (only numeric digits).
    func calibrationValuePart1() -> Int {
        let digits = extractDigits(allowSpelledOut: false)
        guard let first = digits.first, let last = digits.last else { return 0 }
        return first * 10 + last
    }

    // Calculate calibration value for Part 2 (numeric and spelled-out digits).
    func calibrationValuePart2() -> Int {
        let digits = extractDigits(allowSpelledOut: true)
        guard let first = digits.first, let last = digits.last else { return 0 }
        return first * 10 + last
    }
}

// Main function (entry point)
func main() {
    // Read the input file.
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Error reading input file.")
        return
    }

    // Split the input into lines.
    let lines = input.split(separator: "\n").map(String.init)

    // Calculate the sum of calibration values for Part 1.
    let sumPart1 = lines.reduce(0) { total, line in
        total + Calibration(line: line).calibrationValuePart1()
    }
    print("Part 1 Sum: \(sumPart1)")

    // Calculate the sum of calibration values for Part 2.
    let sumPart2 = lines.reduce(0) { total, line in
        total + Calibration(line: line).calibrationValuePart2()
    }
    print("Part 2 Sum: \(sumPart2)")
}

// Run the main function.
main()
