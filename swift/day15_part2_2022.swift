
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
}

struct Sensor {
    let pos: Point
    let beacon: Point
    let dist: Int
}

func manhattan(p: Point, q: Point) -> Int {
    return abs(p.x - q.x) + abs(p.y - q.y)
}

func distress(sensors: [Sensor], maxCoord: Int) -> Int {
    var intervals: [Int: [(Int, Int)]] = [:]
    for s in sensors {
        for y in max(0, s.pos.y - s.dist)...min(maxCoord, s.pos.y + s.dist) {
            let dy = abs(s.pos.y - y)
            let dx = s.dist - dy
            let minX = max(0, s.pos.x - dx)
            let maxX = min(maxCoord, s.pos.x + dx)
            intervals[y, default: []].append((minX, maxX))
        }
    }

    for y in 0...maxCoord {
        var x = 0
        let sortedIntervals = intervals[y, default: []].sorted { $0.0 < $1.0 }
        for (minX, maxX) in sortedIntervals {
            if x < minX {
                return x * 4000000 + y
            }
            x = max(x, maxX + 1)
        }
        if x <= maxCoord {
            return x * 4000000 + y
        }
    }
    return -1
}

func readAll(path: String) -> String {
    do {
        return try String(contentsOfFile: path)
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

let input = readAll(path: "input.txt")
var sensors: [Sensor] = []
for line in input.components(separatedBy: "\n") {
    let scanner = Scanner(string: line)
    var posX = 0, posY = 0, beaconX = 0, beaconY = 0
    scanner.scanString("Sensor at x=")
    scanner.scanInt(&posX)
    scanner.scanString(", y=")
    scanner.scanInt(&posY)
    scanner.scanString(": closest beacon is at x=")
    scanner.scanInt(&beaconX)
    scanner.scanString(", y=")
    scanner.scanInt(&beaconY)
    let pos = Point(x: posX, y: posY)
    let beacon = Point(x: beaconX, y: beaconY)
    let dist = manhattan(p: pos, q: beacon)
    sensors.append(Sensor(pos: pos, beacon: beacon, dist: dist))
}

print(distress(sensors: sensors, maxCoord: 4000000))
