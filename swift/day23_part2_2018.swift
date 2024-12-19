
import Foundation

struct Coordinate: Hashable {
    var x, y, z: Int
    
    static let zero = Coordinate(x: 0, y: 0, z: 0)
    
    func distance(to other: Coordinate) -> Int {
        abs(x - other.x) + abs(y - other.y) + abs(z - other.z)
    }
}

typealias Bots = [Coordinate: [Int]]

func newBots(input: [String]) -> Bots {
    var bots = Bots()
    
    for line in input {
        let parts = line.components(separatedBy: ", ").map { $0.trimmingCharacters(in: CharacterSet(charactersIn: "<>pos=r=")) }
        let coords = parts[0].components(separatedBy: ",").compactMap { Int($0) }
        let radius = Int(parts[1])!
        
        let coord = Coordinate(x: coords[0], y: coords[1], z: coords[2])
        bots[coord, default: []].append(radius)
    }
    
    return bots
}

extension Bots {
    func haveInRange(pos: Coordinate) -> Int {
        var sum = 0
        for (coord, radii) in self {
            for radius in radii {
                if pos.distance(to: coord) <= radius {
                    sum += 1
                }
            }
        }
        return sum
    }
}

func closestSuccess(bots: Bots) -> Int {
    var current = Coordinate.zero
    var topLeft = Coordinate.zero
    var bottomRight = Coordinate.zero
    var zoom = 1 << (Int.bitWidth - 2)
    
    while true {
        var zoomedBots = Bots()
        var best = (pos: Coordinate.zero, count: 0)
        
        for (coord, radii) in bots {
            for radius in radii {
                let zc = Coordinate(x: coord.x / zoom, y: coord.y / zoom, z: coord.z / zoom)
                zoomedBots[zc, default: []].append(radius / zoom)
            }
        }
        
        for x in topLeft.x...bottomRight.x {
            for y in topLeft.y...bottomRight.y {
                for z in topLeft.z...bottomRight.z {
                    let cur = Coordinate(x: x, y: y, z: z)
                    let count = zoomedBots.haveInRange(pos: cur)
                    
                    if count < best.count {
                        continue
                    }
                    if count == best.count && Coordinate.zero.distance(to: cur) >= Coordinate.zero.distance(to: best.pos) {
                        continue
                    }
                    best = (pos: cur, count: count)
                }
            }
        }
        
        topLeft = Coordinate(x: (best.pos.x - 1) << 1, y: (best.pos.y - 1) << 1, z: (best.pos.z - 1) << 1)
        bottomRight = Coordinate(x: (best.pos.x + 1) << 1, y: (best.pos.y + 1) << 1, z: (best.pos.z + 1) << 1)
        zoom >>= 1
        
        if zoom == 0 {
            return Coordinate.zero.distance(to: best.pos)
        }
    }
}

if let input = try? String(contentsOfFile: "input.txt") {
    let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
    let bots = newBots(input: lines)
    print(closestSuccess(bots: bots))
}
