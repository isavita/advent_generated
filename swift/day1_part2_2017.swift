
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)

func solveCaptchaPart1(_ input: String) -> Int {
    var sum = 0
    let digits = input.map { Int(String($0))! }
    
    for i in 0..<digits.count {
        if digits[i] == digits[(i + 1) % digits.count] {
            sum += digits[i]
        }
    }
    
    return sum
}

func solveCaptchaPart2(_ input: String) -> Int {
    var sum = 0
    let digits = input.map { Int(String($0))! }
    
    for i in 0..<digits.count {
        if digits[i] == digits[(i + digits.count/2) % digits.count] {
            sum += digits[i]
        }
    }
    
    return sum
}

let part1 = solveCaptchaPart1(input)
let part2 = solveCaptchaPart2(input)

print(part1)
print(part2)
