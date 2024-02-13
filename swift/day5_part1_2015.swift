
import Foundation

let input = try String(contentsOfFile: "input.txt")
let strings = input.components(separatedBy: .newlines)

func isNiceString(_ str: String) -> Bool {
    let vowels = "aeiou"
    let disallowedStrings = ["ab", "cd", "pq", "xy"]
    
    var vowelCount = 0
    var hasDoubleLetter = false
    var hasDisallowedString = false
    
    var previousChar: Character = " "
    
    for char in str {
        if vowels.contains(char) {
            vowelCount += 1
        }
        
        if char == previousChar {
            hasDoubleLetter = true
        }
        
        let pair = "\(previousChar)\(char)"
        if disallowedStrings.contains(pair) {
            hasDisallowedString = true
        }
        
        previousChar = char
    }
    
    return vowelCount >= 3 && hasDoubleLetter && !hasDisallowedString
}

let niceStringsCount = strings.filter { isNiceString($0) }.count
print(niceStringsCount)
