
import Foundation

struct Point3D: Hashable {
    let x: Int
    let y: Int
    let z: Int

    func rotate(_ rotationIndex: Int) -> Point3D {
        switch rotationIndex {
        case 0: return Point3D(x: x, y: y, z: z)
        case 1: return Point3D(x: x, y: -z, z: y)
        case 2: return Point3D(x: x, y: -y, z: -z)
        case 3: return Point3D(x: x, y: z, z: -y)
        case 4: return Point3D(x: -x, y: -y, z: z)
        case 5: return Point3D(x: -x, y: z, z: y)
        case 6: return Point3D(x: -x, y: y, z: -z)
        case 7: return Point3D(x: -x, y: -z, z: -y)
        case 8: return Point3D(x: y, y: x, z: -z)
        case 9: return Point3D(x: y, y: -z, z: -x)
        case 10: return Point3D(x: y, y: -x, z: z)
        case 11: return Point3D(x: y, y: z, z: x)
        case 12: return Point3D(x: -y, y: -x, z: -z)
        case 13: return Point3D(x: -y, y: -z, z: x)
        case 14: return Point3D(x: -y, y: x, z: z)
        case 15: return Point3D(x: -y, y: z, z: -x)
        case 16: return Point3D(x: z, y: x, z: y)
        case 17: return Point3D(x: z, y: -y, z: x)
        case 18: return Point3D(x: z, y: -x, z: -y)
        case 19: return Point3D(x: z, y: y, z: -x)
        case 20: return Point3D(x: -z, y: -x, z: y)
        case 21: return Point3D(x: -z, y: y, z: x)
        case 22: return Point3D(x: -z, y: x, z: -y)
        case 23: return Point3D(x: -z, y: -y, z: -x)
        default: fatalError("Invalid rotation index")
        }
    }
    
    static func -(lhs: Point3D, rhs: Point3D) -> Point3D {
        return Point3D(x: lhs.x - rhs.x, y: lhs.y - rhs.y, z: lhs.z - rhs.z)
    }

    static func +(lhs: Point3D, rhs: Point3D) -> Point3D {
        return Point3D(x: lhs.x + rhs.x, y: lhs.y + rhs.y, z: lhs.z + rhs.z)
    }
}

func parseInput(from file: String) -> [[Point3D]] {
    let content = try! String(contentsOfFile: file)
    let scannerStrings = content.components(separatedBy: "\n\n")
    return scannerStrings.map { scannerString -> [Point3D] in
        scannerString.split(separator: "\n").dropFirst().map { line -> Point3D in
            let coords = line.split(separator: ",").map { Int($0)! }
            return Point3D(x: coords[0], y: coords[1], z: coords[2])
        }
    }
}

func findOverlap(scanner1: [Point3D], scanner2: [Point3D]) -> (Int, Point3D)? {
    for rotation in 0..<24 {
        let rotatedScanner2 = scanner2.map { $0.rotate(rotation) }
        var offsetCounts: [Point3D: Int] = [:]

        for point1 in scanner1 {
            for point2 in rotatedScanner2 {
                let offset = point1 - point2
                offsetCounts[offset, default: 0] += 1
            }
        }

        if let (offset, count) = offsetCounts.first(where: { $0.value >= 12 }) {
            return (rotation, offset)
        }
    }
    return nil
}

func solve(scanners: [[Point3D]]) -> Set<Point3D> {
    var allBeacons: Set<Point3D> = Set(scanners[0])
    var scannerPositions: [Point3D] = [Point3D(x: 0, y: 0, z: 0)]
    var remainingScanners = Array(scanners.dropFirst())

    while !remainingScanners.isEmpty {
        var foundMatch = false
        for (i, scanner) in remainingScanners.enumerated() {
            if let (rotation, offset) = findOverlap(scanner1: Array(allBeacons), scanner2: scanner) {
                let transformedScanner = scanner.map { $0.rotate(rotation) + offset }
                allBeacons.formUnion(transformedScanner)
                scannerPositions.append(offset)
                remainingScanners.remove(at: i)
                foundMatch = true
                break
            }
        }
        if !foundMatch {
            fatalError("Could not find a match for any remaining scanner.")
        }
    }

    return allBeacons
}

func main() {
    let scanners = parseInput(from: "input.txt")
    let allBeacons = solve(scanners: scanners)
    print(allBeacons.count)
}

main()
