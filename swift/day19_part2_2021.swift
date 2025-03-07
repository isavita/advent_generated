
import Foundation

func readInput(filename: String) -> [[[Int]]] {
    var scanners: [[[Int]]] = []
    if let input = try? String(contentsOfFile: filename) {
        let lines = input.components(separatedBy: "\n")
        var scanner: [[Int]] = []
        for line in lines {
            if line.starts(with: "---") {
                if !scanner.isEmpty {
                    scanners.append(scanner)
                    scanner = []
                }
            } else if !line.isEmpty {
                scanner.append(line.split(separator: ",").map { Int($0)! })
            }
        }
        if !scanner.isEmpty {
            scanners.append(scanner)
        }
    }
    return scanners
}

func getRotations() -> [[[Int]]] {
    var rotations: [[[Int]]] = []
    let permutations = [[0, 1, 2], [0, 2, 1], [1, 0, 2], [1, 2, 0], [2, 0, 1], [2, 1, 0]]
    let signChanges = [[1, 1, 1], [1, 1, -1], [1, -1, 1], [1, -1, -1], [-1, 1, 1], [-1, 1, -1], [-1, -1, 1], [-1, -1, -1]]
    for perm in permutations {
        for signs in signChanges {
            var rot = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
            for i in 0..<3 {
                rot[i][perm[i]] = signs[i]
            }
            let det = rot[0][0] * (rot[1][1] * rot[2][2] - rot[1][2] * rot[2][1])
                    - rot[0][1] * (rot[1][0] * rot[2][2] - rot[1][2] * rot[2][0])
                    + rot[0][2] * (rot[1][0] * rot[2][1] - rot[1][1] * rot[2][0])
            if det == 1 {
                rotations.append(rot)
            }
        }
    }
    return rotations
}

func rotate(_ p: [Int], _ rot: [[Int]]) -> [Int] {
    return [
        p[0] * rot[0][0] + p[1] * rot[0][1] + p[2] * rot[0][2],
        p[0] * rot[1][0] + p[1] * rot[1][1] + p[2] * rot[1][2],
        p[0] * rot[2][0] + p[1] * rot[2][1] + p[2] * rot[2][2]
    ]
}

func add(_ p1: [Int], _ p2: [Int]) -> [Int] {
    return [p1[0] + p2[0], p1[1] + p2[1], p1[2] + p2[2]]
}

func subtract(_ p1: [Int], _ p2: [Int]) -> [Int] {
    return [p1[0] - p2[0], p1[1] - p2[1], p1[2] - p2[2]]
}

func manhattan(_ p1: [Int], _ p2: [Int]) -> Int {
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]) + abs(p1[2] - p2[2])
}

func solve(scanners: [[[Int]]]) -> Int {
    let rotations = getRotations()
    var aligned = Set<Int>([0])
    var scannerPositions = [0: [0, 0, 0]]
    var beacons = Set(scanners[0].map { $0 })
    var pending = Set(1..<scanners.count)

    while !pending.isEmpty {
        var found = false
        for scanner in pending {
            for rot in rotations {
                let rotated = scanners[scanner].map { rotate($0, rot) }
                var deltas = [String: Int]()
                for beacon in rotated {
                    for alignedBeacon in beacons {
                        let delta = subtract(alignedBeacon, beacon)
                        let deltaStr = delta.map(String.init).joined(separator: ",")
                        deltas[deltaStr, default: 0] += 1
                    }
                }

                if let (maxDeltaStr, count) = deltas.max(by: { $0.value < $1.value }), count >= 12 {
                    let maxDelta = maxDeltaStr.split(separator: ",").map { Int($0)! }
                    scannerPositions[scanner] = maxDelta
                    for beacon in rotated {
                        beacons.insert(add(beacon, maxDelta))
                    }
                    aligned.insert(scanner)
                    pending.remove(scanner)
                    found = true
                    break
                }
            }
            if found {
                break
            }
        }
    }

    var maxDistance = 0
    let positions = Array(scannerPositions.values)
    for i in 0..<positions.count {
        for j in (i + 1)..<positions.count {
            let dist = manhattan(positions[i], positions[j])
            maxDistance = max(maxDistance, dist)
        }
    }
    return maxDistance
}

func main() {
    let scanners = readInput(filename: "input.txt")
    let result = solve(scanners: scanners)
    print(result)
}

main()
