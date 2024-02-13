
import Foundation

// Read input from file
let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

// Prepare the repeated input signal
func repeatInput(input: String, times: Int) -> [Int] {
    var digits = [Int]()
    for _ in 0..<times {
        for r in input {
            if let digit = Int(String(r)) {
                digits.append(digit)
            }
        }
    }
    return digits
}

var repeatedInput = repeatInput(input: input, times: 10000)

// Find the message offset
let offset = Int(input.prefix(7))!

// Apply FFT for 100 phases starting from the offset
for _ in 0..<100 {
    var sum = 0
    for i in stride(from: repeatedInput.count - 1, through: offset, by: -1) {
        sum += repeatedInput[i]
        repeatedInput[i] = sum % 10
    }
}

// Print the eight-digit message
for i in offset..<offset+8 {
    print(repeatedInput[i], terminator: "")
}
print()
