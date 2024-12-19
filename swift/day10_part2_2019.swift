
import Foundation

struct Asteroid: Comparable {
    let x: Int
    let y: Int
    let angle: Double
    let dist: Double

    static func < (lhs: Asteroid, rhs: Asteroid) -> Bool {
        if lhs.angle == rhs.angle {
            return lhs.dist < rhs.dist
        }
        return lhs.angle < rhs.angle
    }
}

func readAsteroids(filename: String) -> [[Bool]] {
    guard let fileURL = URL(string: "file://\(FileManager.default.currentDirectoryPath)/\(filename)"),
          let fileContents = try? String(contentsOf: fileURL) else {
        fatalError("Failed to read file")
    }
    return fileContents.components(separatedBy: "\n").filter{!$0.isEmpty}.map { line in
        line.map { $0 == "#" }
    }
}

func vaporizeAsteroids(asteroids: [[Bool]], station: (x: Int, y: Int)) -> [Asteroid] {
    var targets: [Asteroid] = []
    for y in asteroids.indices {
        for x in asteroids[y].indices {
            if asteroids[y][x] && !(x == station.x && y == station.y) {
                var angle = atan2(Double(y - station.y), Double(x - station.x))
                let dist = hypot(Double(x - station.x), Double(y - station.y))
                if angle < -Double.pi / 2 {
                    angle += 2 * Double.pi
                }
                targets.append(Asteroid(x: x, y: y, angle: angle, dist: dist))
            }
        }
    }
    targets.sort()
    var vaporized: [Asteroid] = []
    var lastAngle = -Double.infinity
    while !targets.isEmpty {
        var i = 0
        while i < targets.count {
            if targets[i].angle != lastAngle {
                vaporized.append(targets[i])
                lastAngle = targets[i].angle
                targets.remove(at: i)
            } else {
                i += 1
            }
        }
        lastAngle = -Double.infinity
    }
    return vaporized
}

func findBestAsteroidLocation(asteroids: [[Bool]]) -> ((x: Int, y: Int), maxCount: Int) {
    var bestLocation = (x: 0, y: 0)
    var maxCount = 0
    for y in asteroids.indices {
        for x in asteroids[y].indices {
            if asteroids[y][x] {
                let count = countVisibleAsteroids(asteroids: asteroids, x: x, y: y)
                if count > maxCount {
                    maxCount = count
                    bestLocation = (x: x, y: y)
                }
            }
        }
    }
    return (bestLocation, maxCount)
}

func countVisibleAsteroids(asteroids: [[Bool]], x: Int, y: Int) -> Int {
    var angles: Set<Double> = []
    for otherY in asteroids.indices {
        for otherX in asteroids[otherY].indices {
            if asteroids[otherY][otherX] && !(otherX == x && otherY == y) {
                let angle = atan2(Double(otherY - y), Double(otherX - x))
                angles.insert(angle)
            }
        }
    }
    return angles.count
}

let asteroids = readAsteroids(filename: "input.txt")
let (station, _) = findBestAsteroidLocation(asteroids: asteroids)
let vaporized = vaporizeAsteroids(asteroids: asteroids, station: station)

if vaporized.count >= 200 {
    let result = vaporized[199].x * 100 + vaporized[199].y
    print(result)
} else {
    print("Less than 200 asteroids were vaporized.")
}
