
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
var totalScore = 0

do {
    let input = try String(contentsOf: fileURL).split(separator: "\n")
    
    for line in input {
        let opponent = line[line.startIndex]
        let roundEnd = line[line.index(line.startIndex, offsetBy: 2)]
        
        let yourMove: Character
        switch roundEnd {
        case "X":
            yourMove = opponent == "A" ? "Z" : opponent == "B" ? "X" : "Y"
        case "Y":
            yourMove = opponent == "A" ? "X" : opponent == "B" ? "Y" : "Z"
        default:
            yourMove = opponent == "A" ? "Y" : opponent == "B" ? "Z" : "X"
        }
        
        var score = yourMove == "X" ? 1 : yourMove == "Y" ? 2 : 3
        
        if (opponent == "A" && yourMove == "Y") || (opponent == "B" && yourMove == "Z") || (opponent == "C" && yourMove == "X") {
            score += 6
        } else if (opponent == "A" && yourMove == "X") || (opponent == "B" && yourMove == "Y") || (opponent == "C" && yourMove == "Z") {
            score += 3
        }
        
        totalScore += score
    }
    
    print(totalScore)
} catch {
    print("Error reading file: \(error)")
}
