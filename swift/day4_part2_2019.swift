
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let range = input.components(separatedBy: "-").compactMap { Int($0) }
let lowerBound = range[0]
let upperBound = range[1]

func isValidPasswordPart1(_ password: Int) -> Bool {
    let passwordString = String(password)
    var hasAdjacentDigits = false
    var neverDecrease = true
    
    for i in 0..<passwordString.count-1 {
        let currentDigit = Int(String(passwordString[passwordString.index(passwordString.startIndex, offsetBy: i)]))!
        let nextDigit = Int(String(passwordString[passwordString.index(passwordString.startIndex, offsetBy: i+1)]))!
        
        if currentDigit == nextDigit {
            hasAdjacentDigits = true
        }
        
        if currentDigit > nextDigit {
            neverDecrease = false
        }
    }
    
    return hasAdjacentDigits && neverDecrease
}

func isValidPasswordPart2(_ password: Int) -> Bool {
    let passwordString = String(password)
    var hasAdjacentDigits = false
    var neverDecrease = true
    var adjacentCount = 1
    
    for i in 0..<passwordString.count-1 {
        let currentDigit = Int(String(passwordString[passwordString.index(passwordString.startIndex, offsetBy: i)]))!
        let nextDigit = Int(String(passwordString[passwordString.index(passwordString.startIndex, offsetBy: i+1)]))!
        
        if currentDigit == nextDigit {
            adjacentCount += 1
        } else {
            if adjacentCount == 2 {
                hasAdjacentDigits = true
            }
            adjacentCount = 1
        }
        
        if currentDigit > nextDigit {
            neverDecrease = false
        }
    }
    
    if adjacentCount == 2 {
        hasAdjacentDigits = true
    }
    
    return hasAdjacentDigits && neverDecrease
}

var countPart1 = 0
var countPart2 = 0

for password in lowerBound...upperBound {
    if isValidPasswordPart1(password) {
        countPart1 += 1
    }
    
    if isValidPasswordPart2(password) {
        countPart2 += 1
    }
}

print(countPart1)
print(countPart2)
