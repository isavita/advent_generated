
import Foundation

// Function to calculate the knot hash
func knotHash(_ input: String) -> String {
    var lengths = input.utf8.map { Int($0) }
    lengths += [17, 31, 73, 47, 23]
    var list = Array(0..<256)
    var currentPosition = 0
    var skipSize = 0

    for _ in 0..<64 {
        for length in lengths {
            var i = currentPosition
            var j = (currentPosition + length - 1) % list.count
            for _ in 0..<length / 2 {
                list.swapAt(i, j)
                i = (i + 1) % list.count
                j = (j - 1 + list.count) % list.count
            }
            currentPosition = (currentPosition + length + skipSize) % list.count
            skipSize += 1
        }
    }

    var denseHash = ""
    for i in 0..<16 {
        var xor = 0
        for j in 0..<16 {
            xor ^= list[i * 16 + j]
        }
        denseHash += String(format: "%02x", xor)
    }
    return denseHash
}

// Function to convert hex to binary string
func hexToBinary(_ hex: String) -> String {
    return hex.flatMap {
        guard let intValue = Int(String($0), radix: 16) else { return nil }
        let binaryString = String(intValue, radix: 2)
        return String(repeating: "0", count: 4 - binaryString.count) + binaryString
    }.joined()
}

// Function to count used squares and find regions
func solveDiskDefragmentation(key: String) -> (usedSquares: Int, regions: Int) {
    var grid = [[Int]]()
    var usedSquares = 0

    for i in 0..<128 {
        let hashInput = "\(key)-\(i)"
        let hash = knotHash(hashInput)
        let binary = hexToBinary(hash)
        let row = binary.map { Int(String($0))! }
        usedSquares += row.reduce(0, +)
        grid.append(row)
    }

    func dfs(row: Int, col: Int) {
        guard row >= 0 && row < 128 && col >= 0 && col < 128 && grid[row][col] == 1 else { return }
        grid[row][col] = 0
        dfs(row: row + 1, col: col)
        dfs(row: row - 1, col: col)
        dfs(row: row, col: col + 1)
        dfs(row: row, col: col - 1)
    }

    var regions = 0
    for row in 0..<128 {
        for col in 0..<128 {
            if grid[row][col] == 1 {
                regions += 1
                dfs(row: row, col: col)
            }
        }
    }

    return (usedSquares, regions)
}

// Read input from file
guard let input = try? String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines) else {
    fatalError("Could not read input file")
}

// Solve the problem
let (usedSquares, regions) = solveDiskDefragmentation(key: input)

// Print the results
print("Used squares: \(usedSquares)")
print("Regions: \(regions)")
