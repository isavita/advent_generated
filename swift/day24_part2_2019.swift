
import Foundation

let side = 5
let square = side * side

func parse() -> [Bool] {
    var res = [Bool](repeating: false, count: square)
    if let file = FileHandle(forReadingAtPath: "input.txt") {
        let data = file.readDataToEndOfFile()
        let lines = String(data: data, encoding: .utf8)?.split(separator: "\n") ?? []
        for (row, line) in lines.enumerated() {
            for col in 0..<side {
                res[row * side + col] = line[line.index(line.startIndex, offsetBy: col)] == "#"
            }
        }
    }
    return res
}

typealias Space = [Int: [Bool]]

func next(space: Space) -> Space {
    var newSpace = Space()
    let (minLevel, maxLevel) = minMaxLevel(space)

    for level in (minLevel - 1)...(maxLevel + 1) {
        var grid = [Bool](repeating: false, count: square)
        
        for cell in 0..<square {
            if cell == 12 { continue }
            let (row, col) = (cell / side, cell % side)
            var neighbours = 0
            
            if row == 0 && infested(space: space, level: level - 1, cell: 7) { neighbours += 1 }
            if col == 0 && infested(space: space, level: level - 1, cell: 11) { neighbours += 1 }
            if col == 4 && infested(space: space, level: level - 1, cell: 13) { neighbours += 1 }
            if row == 4 && infested(space: space, level: level - 1, cell: 17) { neighbours += 1 }
            
            if cell == 7 {
                for i in 0..<side { if infested(space: space, level: level + 1, cell: i) { neighbours += 1 } }
            }
            if cell == 11 {
                for i in 0..<side { if infested(space: space, level: level + 1, cell: 5 * i) { neighbours += 1 } }
            }
            if cell == 13 {
                for i in 0..<side { if infested(space: space, level: level + 1, cell: 5 * i + side - 1) { neighbours += 1 } }
            }
            if cell == 17 {
                for i in 0..<side { if infested(space: space, level: level + 1, cell: (side - 1) * side + i) { neighbours += 1 } }
            }
            
            if row > 0 && cell != 17 && infested(space: space, level: level, cell: cell - side) { neighbours += 1 }
            if col > 0 && cell != 13 && infested(space: space, level: level, cell: cell - 1) { neighbours += 1 }
            if col < side - 1 && cell != 11 && infested(space: space, level: level, cell: cell + 1) { neighbours += 1 }
            if row < side - 1 && cell != 7 && infested(space: space, level: level, cell: cell + side) { neighbours += 1 }
            
            grid[cell] = (infested(space: space, level: level, cell: cell) && neighbours == 1) || (!infested(space: space, level: level, cell: cell) && (neighbours == 1 || neighbours == 2))
        }
        
        newSpace[level] = grid
    }
    
    clean(space: &newSpace)
    return newSpace
}

func clean(space: inout Space) {
    let (min, max) = minMaxLevel(space)
    if space[min]?.contains(true) == false { space.removeValue(forKey: min) }
    if space[max]?.contains(true) == false { space.removeValue(forKey: max) }
}

func infested(space: Space, level: Int, cell: Int) -> Bool {
    return space[level]?[cell] ?? false
}

func minMaxLevel(_ space: Space) -> (Int, Int) {
    return space.keys.reduce((Int.max, Int.min)) { (minMax, level) in
        (min(minMax.0, level), max(minMax.1, level))
    }
}

let input = parse()
var space: Space = [0: input]

for _ in 0..<200 {
    space = next(space: space)
}

let count = space.values.flatMap { $0 }.filter { $0 }.count
print(count)
