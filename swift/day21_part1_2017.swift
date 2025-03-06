
import Foundation

// Function to rotate a grid 90 degrees clockwise
func rotate(_ grid: [[Character]]) -> [[Character]] {
    let n = grid.count
    var rotated = [[Character]](repeating: [Character](repeating: " ", count: n), count: n)
    for i in 0..<n {
        for j in 0..<n {
            rotated[j][n - 1 - i] = grid[i][j]
        }
    }
    return rotated
}

// Function to flip a grid horizontally
func flip(_ grid: [[Character]]) -> [[Character]] {
    return grid.map { $0.reversed() }
}

// Function to convert a grid to string representation
func gridToString(_ grid: [[Character]]) -> String {
    return grid.map { String($0) }.joined(separator: "/")
}

// Function to convert a string to grid representation
func stringToGrid(_ str: String) -> [[Character]] {
    return str.split(separator: "/").map { Array($0) }
}

// Function to enhance the image
func enhance(_ initialGrid: [[Character]], _ rules: [String: String]) -> [[Character]] {
    let size = initialGrid.count
    let blockSize = (size % 2 == 0) ? 2 : 3
    let newBlockSize = (size % 2 == 0) ? 3 : 4
    let blocksPerRow = size / blockSize
    let newSize = blocksPerRow * newBlockSize

    var newGrid = [[Character]](repeating: [Character](repeating: " ", count: newSize), count: newSize)

    for row in 0..<blocksPerRow {
        for col in 0..<blocksPerRow {
            var block = [[Character]]()
            for i in 0..<blockSize {
                block.append(Array(initialGrid[row * blockSize + i][col * blockSize..<(col * blockSize + blockSize)]))
            }

            var enhancedBlockStr = ""
            var found = false
            
            var currentBlock = block
            for _ in 0..<4 { // Check rotations
                let blockStr = gridToString(currentBlock)
                if let rule = rules[blockStr] {
                    enhancedBlockStr = rule
                    found = true
                    break
                }
                
                let flippedBlockStr = gridToString(flip(currentBlock))
                if let rule = rules[flippedBlockStr] {
                    enhancedBlockStr = rule
                    found = true
                    break
                }
                currentBlock = rotate(currentBlock)
            }

            if !found {
                 let blockStr = gridToString(currentBlock)
                fatalError("No rule found for block: \(blockStr)")
            }
            
            let enhancedBlock = stringToGrid(enhancedBlockStr)

            for i in 0..<newBlockSize {
                for j in 0..<newBlockSize {
                    newGrid[row * newBlockSize + i][col * newBlockSize + j] = enhancedBlock[i][j]
                }
            }
        }
    }
    return newGrid
}


func main() {
    // Read rules from input.txt
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Could not read input.txt")
    }

    var rules: [String: String] = [:]
    input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n").forEach { line in
        let parts = line.split(separator: " => ")
        rules[String(parts[0])] = String(parts[1])
    }

    // Initial pattern
    var grid = stringToGrid(".#./..#/###")

    // Enhance the image for 5 iterations
    for _ in 0..<5 {
        grid = enhance(grid, rules)
    }

    // Count the number of pixels that are on
    let onPixels = grid.flatMap { $0 }.filter { $0 == "#" }.count
    print(onPixels)
}

main()
