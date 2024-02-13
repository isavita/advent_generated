
import Foundation

struct Coordinate: Hashable {
    var x: Int
    var y: Int
    var z: Int
}

var activeCubes = [Coordinate: Bool]()

if let input = try? String(contentsOfFile: "input.txt") {
    let initialState = input.components(separatedBy: "\n").map { $0.trimmingCharacters(in: .whitespaces) }
    
    for (y, line) in initialState.enumerated() {
        for (x, char) in line.enumerated() {
            if char == "#" {
                activeCubes[Coordinate(x: x, y: y, z: 0)] = true
            }
        }
    }
    
    for _ in 0..<6 {
        activeCubes = simulateCycle(activeCubes)
    }
    
    print(activeCubes.count)
}

func simulateCycle(_ activeCubes: [Coordinate: Bool]) -> [Coordinate: Bool] {
    var newActiveCubes = [Coordinate: Bool]()
    var neighborCounts = [Coordinate: Int]()
    
    for (coord, _) in activeCubes {
        for dz in -1...1 {
            for dy in -1...1 {
                for dx in -1...1 {
                    if dz == 0 && dy == 0 && dx == 0 {
                        continue
                    }
                    let neighbor = Coordinate(x: coord.x + dx, y: coord.y + dy, z: coord.z + dz)
                    neighborCounts[neighbor, default: 0] += 1
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
