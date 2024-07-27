
import Foundation

// Function to read the input from the file and return the risk map
func readInput(from filename: String) -> [[Int]] {
    let url = URL(fileURLWithPath: filename)
    let content = try! String(contentsOf: url)
    let lines = content.split(separator: "\n").map { String($0) }
    return lines.map { $0.compactMap { Int(String($0)) } }
}

// Function to create the full risk map (5x5 tiled)
func createFullMap(originalMap: [[Int]]) -> [[Int]] {
    let originalSize = originalMap.count
    let fullSize = originalSize * 5
    var fullMap = Array(repeating: Array(repeating: 0, count: fullSize), count: fullSize)
    
    for i in 0..<fullSize {
        for j in 0..<fullSize {
            let originalRisk = originalMap[i % originalSize][j % originalSize]
            let increment = (i / originalSize) + (j / originalSize)
            fullMap[i][j] = (originalRisk + increment - 1) % 9 + 1
        }
    }
    
    return fullMap
}

// Dijkstra's algorithm to find the lowest risk path
func dijkstra(map: [[Int]]) -> Int {
    let size = map.count
    var riskLevels = Array(repeating: Array(repeating: Int.max, count: size), count: size)
    riskLevels[0][0] = 0
    
    var priorityQueue: [(Int, Int, Int)] = [(0, 0, 0)] // (risk, x, y)
    
    while !priorityQueue.isEmpty {
        priorityQueue.sort { $0.0 < $1.0 }
        let (currentRisk, x, y) = priorityQueue.removeFirst()
        
        if x == size - 1 && y == size - 1 {
            return currentRisk
        }
        
        let directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
        
        for (dx, dy) in directions {
            let newX = x + dx
            let newY = y + dy
            
            if newX >= 0 && newX < size && newY >= 0 && newY < size {
                let newRisk = currentRisk + map[newX][newY]
                if newRisk < riskLevels[newX][newY] {
                    riskLevels[newX][newY] = newRisk
                    priorityQueue.append((newRisk, newX, newY))
                }
            }
        }
    }
    
    return -1 // Should never reach here if the input is valid
}

// Main execution
let originalMap = readInput(from: "input.txt")
let fullMap = createFullMap(originalMap: originalMap)
let lowestRisk = dijkstra(map: fullMap)
print("The lowest total risk of any path from the top left to the bottom right is: \(lowestRisk)")
