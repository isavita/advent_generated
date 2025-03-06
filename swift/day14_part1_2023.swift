
import Foundation

// Function to tilt the platform to the North
func tiltNorth(_ platform: inout [[Character]]) {
    let rows = platform.count
    let cols = platform[0].count

    for col in 0..<cols {
        var nextOpenRow = 0
        for row in 0..<rows {
            if platform[row][col] == "O" {
                platform[row][col] = "."
                platform[nextOpenRow][col] = "O"
                nextOpenRow += 1
            } else if platform[row][col] == "#" {
                nextOpenRow = row + 1
            }
        }
    }
}

// Function to calculate the load on the North support beams
func calculateNorthLoad(_ platform: [[Character]]) -> Int {
    var totalLoad = 0
    let rows = platform.count

    for (rowIndex, row) in platform.enumerated() {
        for element in row {
            if element == "O" {
                totalLoad += (rows - rowIndex)
            }
        }
    }
    return totalLoad
}

func main() {
    // Read input from file
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Failed to read input file")
        return
    }
    
    // Convert input string to a 2D array (platform)
    var platform = input.trimmingCharacters(in: .whitespacesAndNewlines)
                        .components(separatedBy: .newlines)
                        .map { Array($0) }

    // Tilt the platform to the North
    tiltNorth(&platform)

    // Calculate the total load on the North support beams
    let totalLoad = calculateNorthLoad(platform)

    // Print the result
    print(totalLoad)
}

// Start the execution
main()
