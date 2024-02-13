
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

func countTrees(right: Int, down: Int) -> Int {
    var treesEncountered = 0
    var x = 0
    var y = 0
    
    while y < lines.count {
        let line = lines[y]
        let position = line.index(line.startIndex, offsetBy: x % line.count)
        
        if line[position] == "#" {
            treesEncountered += 1
        }
        
        x += right
        y += down
    }
    
    return treesEncountered
}

let result = countTrees(right: 1, down: 1) * countTrees(right: 3, down: 1) * countTrees(right: 5, down: 1) * countTrees(right: 7, down: 1) * countTrees(right: 1, down: 2)
print(result)
