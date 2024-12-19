
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

func impossible(sensors: [Sensor], y: Int) -> Int {
    var intervals: [(Int, Int)] = []
    var beaconsOnLine: Set<Int> = []

    for s in sensors {
        let dy = abs(s.pos.y - y)
        let dx = s.dist - dy
        if dx >= 0 {
            intervals.append((s.pos.x - dx, s.pos.x + dx))
        }
        if s.beacon.y == y {
            beaconsOnLine.insert(s.beacon.x)
        }
    }

    intervals.sort { $0.0 < $1.0 }
    var merged: [(Int, Int)] = []
    for interval in intervals {
        if let last = merged.last, interval.0 <= last.1 {
            merged[merged.count - 1].1 = max(last.1, interval.1)
        } else {
            merged.append(interval)
        }
    }

    var count = 0
    for interval in merged {
        count += interval.1 - interval.0 + 1
    }

    for beaconX in beaconsOnLine {
        if merged.contains(where: { $0.0 <= beaconX && beaconX <= $0.1 }) {
            count -= 1
        }
    }

    return count
}

let input = try! String(contentsOfFile: "input.txt")
var sensors: [Sensor] = []
for line in input.split(separator: "\n") {
    let scanner = Scanner(string: String(line))
    scanner.charactersToBeSkipped = CharacterSet.decimalDigits.inverted
    var sx = 0, sy = 0, bx = 0, by = 0
    scanner.scanInt(&sx)
    scanner.scanInt(&sy)
    scanner.scanInt(&bx)
    scanner.scanInt(&by)
    let s = Sensor(pos: Point(x: sx, y: sy), beacon: Point(x: bx, y: by), dist: manhattan(p: Point(x: sx, y: sy), q: Point(x: bx, y: by)))
    sensors.append(s)
}

print(impossible(sensors: sensors, y: 2000000))
