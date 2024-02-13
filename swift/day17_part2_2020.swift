
import Foundation

struct Coordinate4D: Hashable {
    var x, y, z, w: Int
}

if let input = try? String(contentsOfFile: "input.txt") {
    let initialState = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
    var activeCubes = [Coordinate4D: Bool]()

    for (y, line) in initialState.enumerated() {
        for (x, char) in line.enumerated() {
            if char == "#" {
                activeCubes[Coordinate4D(x: x, y: y, z: 0, w: 0)] = true
            }
        }
    }

    for _ in 0..<6 {
        activeCubes = simulateCycle4D(activeCubes)
    }

    print(activeCubes.count)
}

func simulateCycle4D(_ activeCubes: [Coordinate4D: Bool]) -> [Coordinate4D: Bool] {
    var newActiveCubes = [Coordinate4D: Bool]()
    var neighborCounts = [Coordinate4D: Int]()

    for coord in activeCubes.keys {
        for dw in -1...1 {
            for dz in -1...1 {
                for dy in -1...1 {
                    for dx in -1...1 {
                        if dw == 0 && dz == 0 && dy == 0 && dx == 0 {
                            continue
                        }
                        let neighbor = Coordinate4D(x: coord.x + dx, y: coord.y + dy, z: coord.z + dz, w: coord.w + dw)
                        neighborCounts[neighbor, default: 0] += 1
                    }
                }
            }
        }
    }

    for (coord, count) in neighborCounts {
        if count == 3 || (count == 2 && activeCubes[coord] == true) {
            newActiveCubes[coord] = true
        }
    }

    return newActiveCubes
}
