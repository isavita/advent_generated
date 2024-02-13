
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let range = input.components(separatedBy: "-").compactMap { Int($0) }
let lowerBound = range[0]
let upperBound = range[1]

func isValidPassword(_ password: Int) -> Bool {
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
            break
        }
    }
    
    return hasAdjacentDigits && neverDecrease
}

var count = 0
for password in lowerBound...upperBound {
    if isValidPassword(password) {
        count += 1
    }
}

print(count)
