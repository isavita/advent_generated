
import Foundation

func isNicePart1(_ str: String) -> Bool {
    let vowels = "aeiou"
    let forbidden = ["ab", "cd", "pq", "xy"]
    
    var vowelCount = 0
    var hasDoubleLetter = false
    var hasForbiddenString = false
    
    var prevChar: Character = " "
    var prevPrevChar: Character = " "
    
    for char in str {
        if vowels.contains(char) {
            vowelCount += 1
        }
        
        if char == prevChar {
            hasDoubleLetter = true
        }
        
        let pair = "\(prevPrevChar)\(prevChar)"
        if forbidden.contains(pair) {
            hasForbiddenString = true
        }
        
        prevPrevChar = prevChar
        prevChar = char
    }
    
    return vowelCount >= 3 && hasDoubleLetter && !hasForbiddenString
}

func isNicePart2(_ str: String) -> Bool {
    var hasPairRepeat = false
    var hasLetterRepeat = false
    
    for i in 0..<(str.count - 1) {
        let pair = str.index(str.startIndex, offsetBy: i)
        let nextPair = str.index(after: pair)
        
        if str[nextPair...].contains(str[pair...]) {
            hasPairRepeat = true
        }
        
        if i < str.count - 2 && str[pair] == str[str.index(str.startIndex, offsetBy: i + 2)] {
            hasLetterRepeat = true
        }
    }
    
    return hasPairRepeat && hasLetterRepeat
}

let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
let strings = input.components(separatedBy: .newlines)

let niceStringsPart1 = strings.filter { isNicePart1($0) }.count
print(niceStringsPart1)

let niceStringsPart2 = strings.filter { isNicePart2($0) }.count
print(niceStringsPart2)
