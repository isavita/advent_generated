import Foundation

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let grid = fileContent.components(separatedBy: "\n").map { Array($0) }

        var x = 0, y = 0
        for i in 0..<grid[0].count {
            if grid[0][i] == "|" {
                x = i
                break
            }
        }

        var dx = 0, dy = 1
        var steps = 0

        while true {
            if x < 0 || x >= grid[0].count || y < 0 || y >= grid.count {
                break
            }

            let cell = grid[y][x]

            if cell == " " {
                break
            }

            steps += 1

            if cell == "+" {
                if dx == 0 {
                    if x > 0 && (grid[y][x-1] == "-" || (grid[y][x-1].unicodeScalars.first!.value >= 65 && grid[y][x-1].unicodeScalars.first!.value <= 90)) {
                        dx = -1
                        dy = 0
                    } else {
                        dx = 1
                        dy = 0
                    }
                } else {
                    if y > 0 && (grid[y-1][x] == "|" || (grid[y-1][x].unicodeScalars.first!.value >= 65 && grid[y-1][x].unicodeScalars.first!.value <= 90)) {
                        dx = 0
                        dy = -1
                    } else {
                        dx = 0
                        dy = 1
                    }
                }
            }

            x += dx
            y += dy
        }

        print(steps)
    } catch {
        print("Error reading file")
    }
}

main()