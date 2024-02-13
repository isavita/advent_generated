
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)

func isTrap(_ left: Character, _ center: Character, _ right: Character) -> Character {
    if (left == "^" && center == "^" && right == ".") ||
       (center == "^" && right == "^" && left == ".") ||
       (left == "^" && center == "." && right == ".") ||
       (right == "^" && center == "." && left == ".") {
        return "^"
    } else {
        return "."
    }
}

var currentRow = input
var safeTileCount = currentRow.filter { $0 == "." }.count

for _ in 1...39 {
    var nextRow = ""
    for i in 0..<currentRow.count {
        let left = i == 0 ? "." : String(currentRow[currentRow.index(currentRow.startIndex, offsetBy: i-1)])
        let center = String(currentRow[currentRow.index(currentRow.startIndex, offsetBy: i)])
        let right = i == currentRow.count - 1 ? "." : String(currentRow[currentRow.index(currentRow.startIndex, offsetBy: i+1)])
        
        nextRow.append(isTrap(Character(left), Character(center), Character(right)))
    }
    
    safeTileCount += nextRow.filter { $0 == "." }.count
    currentRow = nextRow
}

print(safeTileCount)
