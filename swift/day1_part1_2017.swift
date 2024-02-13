
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let digits = input.map { Int(String($0)) ?? 0 }

var sum = 0
for i in 0..<digits.count {
    if digits[i] == digits[(i + 1) % digits.count] {
        sum += digits[i]
    }
}

print(sum)
