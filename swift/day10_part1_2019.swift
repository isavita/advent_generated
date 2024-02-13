
import Foundation

func readAsteroids(filename: String) -> [[Bool]] {
    let fileURL = URL(fileURLWithPath: filename)
    let data = try! String(contentsOf: fileURL)
    var asteroids: [[Bool]] = []
    data.enumerateLines { line, _ in
        let asteroidRow = line.map { $0 == "#" }
        asteroids.append(asteroidRow)
    }
    return asteroids
}

func findBestAsteroidLocation(asteroids: [[Bool]]) -> Int {
    var maxCount = 0
    for y in 0..<asteroids.count {
        for x in 0..<asteroids[y].count {
            if asteroids[y][x] {
                let count = countVisibleAsteroids(asteroids: asteroids, x: x, y: y)
                if count > maxCount {
                    maxCount = count
                }
            }
        }
    }
    return maxCount
}

func countVisibleAsteroids(asteroids: [[Bool]], x: Int, y: Int) -> Int {
    var angles: Set<Double> = []
    for otherY in 0..<asteroids.count {
        for otherX in 0..<asteroids[otherY].count {
            if asteroids[otherY][otherX] && !(otherX == x && otherY == y) {
                let angle = atan2(Double(otherY - y), Double(otherX - x))
                angles.insert(angle)
            }
        }
    }
    return angles.count
}

let asteroids = readAsteroids(filename: "input.txt")
let maxCount = findBestAsteroidLocation(asteroids: asteroids)
print(maxCount)
