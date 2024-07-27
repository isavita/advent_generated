
import Foundation

// Function to read the input from a file
func readInput(from file: String) -> [[Character]] {
    do {
        let content = try String(contentsOfFile: file)
        return content.split(separator: "\n").map { Array($0) }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

// Function to count occupied adjacent seats
func occupiedAdjacentSeats(seats: [[Character]], row: Int, col: Int) -> Int {
    let directions = [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1),          (0, 1),
        (1, -1), (1, 0), (1, 1)
    ]
    
    var count = 0
    for (dx, dy) in directions {
        let newRow = row + dx
        let newCol = col + dy
        if newRow >= 0, newRow < seats.count, newCol >= 0, newCol < seats[0].count {
            if seats[newRow][newCol] == "#" {
                count += 1
            }
        }
    }
    return count
}

// Function to simulate the seating system
func simulateSeating(seats: [[Character]]) -> [[Character]] {
    var currentSeats = seats
    var changed = true
    
    while changed {
        changed = false
        var newSeats = currentSeats
        
        for row in 0..<currentSeats.count {
            for col in 0..<currentSeats[row].count {
                if currentSeats[row][col] == "L" && occupiedAdjacentSeats(seats: currentSeats, row: row, col: col) == 0 {
                    newSeats[row][col] = "#"
                    changed = true
                } else if currentSeats[row][col] == "#" && occupiedAdjacentSeats(seats: currentSeats, row: row, col: col) >= 4 {
                    newSeats[row][col] = "L"
                    changed = true
                }
            }
        }
        
        currentSeats = newSeats
    }
    
    return currentSeats
}

// Function to count occupied seats
func countOccupiedSeats(seats: [[Character]]) -> Int {
    return seats.flatMap { $0 }.filter { $0 == "#" }.count
}

// Main execution
let inputFile = "input.txt"
let initialSeats = readInput(from: inputFile)
let finalSeats = simulateSeating(seats: initialSeats)
let occupiedCount = countOccupiedSeats(seats: finalSeats)

print("Number of occupied seats: \(occupiedCount)")
