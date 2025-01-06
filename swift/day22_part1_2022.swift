
import Foundation

// Read input from file
guard let input = try? String(contentsOfFile: "input.txt") else {
    fatalError("Could not read input file")
}

// Parse input into map and path
let parts = input.components(separatedBy: "\n\n")
let mapLines = parts[0].split(separator: "\n").map { Array(String($0)) }
let pathString = parts[1].trimmingCharacters(in: .whitespacesAndNewlines)

// Determine map dimensions and pad with spaces for easier wrapping
let maxRowLength = mapLines.map { $0.count }.max()!
let map: [[Character]] = mapLines.map { row in
    var paddedRow = row
    while paddedRow.count < maxRowLength {
        paddedRow.append(" ")
    }
    return paddedRow
}

// Parse path into instructions
var instructions: [(Int, Character?)] = []
var currentNumber = ""
for char in pathString {
    if char.isNumber {
        currentNumber.append(char)
    } else {
        instructions.append((Int(currentNumber)!, char))
        currentNumber = ""
    }
}
if !currentNumber.isEmpty {
    instructions.append((Int(currentNumber)!, nil))
}

// Define directions and initial position
let directions: [(Int, Int)] = [(0, 1), (1, 0), (0, -1), (-1, 0)] // R, D, L, U
var row = 0
var col = map[0].firstIndex(of: ".")!
var facing = 0

// Function to find the next valid position after wrapping
func findWrappedPosition(row: Int, col: Int, direction: (Int, Int)) -> (Int, Int) {
    var newRow = row
    var newCol = col
    while true {
        newRow = (newRow + direction.0 + map.count) % map.count
        newCol = (newCol + direction.1 + maxRowLength) % maxRowLength
        if map[newRow][newCol] != " " {
            return (newRow, newCol)
        }
    }
}

// Follow the path
for (steps, turn) in instructions {
    for _ in 0..<steps {
        let nextRow = (row + directions[facing].0 + map.count) % map.count
        let nextCol = (col + directions[facing].1 + maxRowLength) % maxRowLength
        
        if map[nextRow][nextCol] == "#" {
            break // Hit a wall
        } else if map[nextRow][nextCol] == "." {
            row = nextRow
            col = nextCol
        } else { // Wrap around
            let (wrappedRow, wrappedCol) = findWrappedPosition(row: row, col: col, direction: directions[facing])
            if map[wrappedRow][wrappedCol] == "#" {
                break // Hit a wall after wrapping
            } else {
                row = wrappedRow
                col = wrappedCol
            }
        }
    }
    
    if let turn = turn {
        if turn == "R" {
            facing = (facing + 1) % 4
        } else {
            facing = (facing + 3) % 4
        }
    }
}

// Calculate final password
let finalPassword = 1000 * (row + 1) + 4 * (col + 1) + facing
print(finalPassword)
