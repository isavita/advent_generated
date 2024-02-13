
import Foundation

let input = try String(contentsOfFile: "input.txt")
var grid = input.components(separatedBy: "\n").map { Array($0) }

func getBiodiversityRating(_ grid: [[Character]]) -> Int {
    var seenLayouts = Set<String>()
    var currentGrid = grid
    
    while true {
        let layout = currentGrid.flatMap { $0 }.map { String($0) }.joined()
        
        if seenLayouts.contains(layout) {
            return currentGrid.flatMap { $0 }.enumerated().reduce(0) { result, arg in
                let (index, char) = arg
                return char == "#" ? result + Int(pow(2.0, Double(index))) : result
            }
        }
        
        seenLayouts.insert(layout)
        
        var newGrid = currentGrid
        
        for i in 0..<currentGrid.count {
            for j in 0..<currentGrid[i].count {
                var adjacentBugs = 0
                
                if i > 0 && currentGrid[i - 1][j] == "#" { adjacentBugs += 1 }
                if i < currentGrid.count - 1 && currentGrid[i + 1][j] == "#" { adjacentBugs += 1 }
                if j > 0 && currentGrid[i][j - 1] == "#" { adjacentBugs += 1 }
                if j < currentGrid[i].count - 1 && currentGrid[i][j + 1] == "#" { adjacentBugs += 1 }
                
                if currentGrid[i][j] == "#" {
                    newGrid[i][j] = adjacentBugs == 1 ? "#" : "."
                } else {
                    newGrid[i][j] = (adjacentBugs == 1 || adjacentBugs == 2) ? "#" : "."
                }
            }
        }
        
        currentGrid = newGrid
    }
}

print(getBiodiversityRating(grid))
