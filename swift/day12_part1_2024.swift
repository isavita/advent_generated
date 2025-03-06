
import Foundation

func calculateRegionProperties(grid: [[Character]], visited: inout [[Bool]], row: Int, col: Int, plantType: Character) -> (area: Int, perimeter: Int) {
    let rows = grid.count
    let cols = grid[0].count

    if row < 0 || row >= rows || col < 0 || col >= cols || visited[row][col] || grid[row][col] != plantType {
        return (0, 0)
    }

    visited[row][col] = true
    var area = 1
    var perimeter = 0

    let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

    for (dr, dc) in directions {
        let newRow = row + dr
        let newCol = col + dc

        if newRow < 0 || newRow >= rows || newCol < 0 || newCol >= cols || grid[newRow][newCol] != plantType {
            perimeter += 1
        }
    }

    for (dr, dc) in directions {
        let newRow = row + dr
        let newCol = col + dc
        let neighborProperties = calculateRegionProperties(grid: grid, visited: &visited, row: newRow, col: newCol, plantType: plantType)
        area += neighborProperties.area
        perimeter += neighborProperties.perimeter // Corrected:  Sum perimeters of recursive calls.
    }


    return (area, perimeter)
}

func solve() {
    do {
        // Read the input file
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: .newlines)
        let grid = lines.map { Array($0) }

        var visited = Array(repeating: Array(repeating: false, count: grid[0].count), count: grid.count)
        var totalPrice = 0

        for row in 0..<grid.count {
            for col in 0..<grid[0].count {
                if !visited[row][col] {
                    let plantType = grid[row][col]
                    let (area, perimeter) = calculateRegionProperties(grid: grid, visited: &visited, row: row, col: col, plantType: plantType)
                    totalPrice += area * perimeter
                }
            }
        }

        print(totalPrice)

    } catch {
        print("Error reading file: \(error)")
    }
}


// Create a main function to call the solve function for proper structure and clarity.
func main() {
    solve()
}
main() // Entry point for the program
