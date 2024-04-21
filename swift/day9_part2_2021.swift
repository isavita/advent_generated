import Foundation

func main() {
    do {
        let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let heightmap = fileContent.components(separatedBy: "\n").map { row in
            Array(row).compactMap { Int(String($0)) }
        }
        
        var basinSizes: [Int] = []
        var visited: [(Int, Int)] = []
        
        for y in 0..<heightmap.count {
            for x in 0..<heightmap[y].count {
                if isLowPoint(heightmap, x: x, y: y) {
                    var localVisited: [(Int, Int)] = []
                    let size = exploreBasin(heightmap, x: x, y: y, visited: &localVisited)
                    basinSizes.append(size)
                    visited.append(contentsOf: localVisited)
                }
            }
        }
        
        basinSizes.sort { $0 > $1 }
        let result = basinSizes[0] * basinSizes[1] * basinSizes[2]
        print(result)
    } catch {
        print("Error: \(error)")
    }
}

func isLowPoint(_ heightmap: [[Int]], x: Int, y: Int) -> Bool {
    let height = heightmap[y][x]
    if x > 0 && heightmap[y][x-1] <= height { return false }
    if x < heightmap[y].count - 1 && heightmap[y][x+1] <= height { return false }
    if y > 0 && heightmap[y-1][x] <= height { return false }
    if y < heightmap.count - 1 && heightmap[y+1][x] <= height { return false }
    return true
}

func exploreBasin(_ heightmap: [[Int]], x: Int, y: Int, visited: inout [(Int, Int)]) -> Int {
    if visited.contains(where: { $0 == (x, y) }) || heightmap[y][x] == 9 { return 0 }
    visited.append((x, y))
    var size = 1
    
    let directions: [(Int, Int)] = [(0, -1), (-1, 0), (0, 1), (1, 0)]
    for direction in directions {
        let newX = x + direction.0
        let newY = y + direction.1
        if newX >= 0 && newX < heightmap[0].count && newY >= 0 && newY < heightmap.count {
            size += exploreBasin(heightmap, x: newX, y: newY, visited: &visited)
        }
    }
    return size
}

main()