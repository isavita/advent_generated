import Foundation

struct Coordinate: Hashable {
    var q: Int
    var r: Int
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(q)
        hasher.combine(r)
    }
    
    static func == (lhs: Coordinate, rhs: Coordinate) -> Bool {
        return lhs.q == rhs.q && lhs.r == rhs.r
    }
}

let directions: [String: Coordinate] = [
    "e": Coordinate(q: 1, r: 0),
    "se": Coordinate(q: 0, r: 1),
    "sw": Coordinate(q: -1, r: 1),
    "w": Coordinate(q: -1, r: 0),
    "nw": Coordinate(q: 0, r: -1),
    "ne": Coordinate(q: 1, r: -1)
]

func getNeighbors(_ tile: Coordinate) -> [Coordinate] {
    var neighbors: [Coordinate] = []
    for dir in directions.values {
        neighbors.append(Coordinate(q: tile.q + dir.q, r: tile.r + dir.r))
    }
    return neighbors
}

func main() {
    do {
        guard let file = FileManager.default.contents(atPath: "input.txt") else {
            fatalError("Failed to open file")
        }
        
        var blackTiles: [Coordinate: Bool] = [:]
        let lines = String(data: file, encoding: .utf8)!.components(separatedBy: "\n")
        
        for line in lines {
            var coord = Coordinate(q: 0, r: 0)
            var i = 0
            while i < line.count {
                var dir: String
                if line[line.index(line.startIndex, offsetBy: i)] == "e" || line[line.index(line.startIndex, offsetBy: i)] == "w" {
                    dir = String(line[line.index(line.startIndex, offsetBy: i)])
                } else {
                    dir = String(line[line.index(line.startIndex, offsetBy: i)..<line.index(line.startIndex, offsetBy: i+2)])
                    i += 1
                }
                let move = directions[dir]!
                coord.q += move.q
                coord.r += move.r
                i += 1
            }
            blackTiles[coord] = !(blackTiles[coord] ?? false)
        }
        
        for _ in 0..<100 {
            var tilesToCheck: [Coordinate: Bool] = [:]
            for tile in blackTiles.keys {
                if blackTiles[tile]! {
                    tilesToCheck[tile] = true
                    for neighbor in getNeighbors(tile) {
                        tilesToCheck[neighbor] = true
                    }
                }
            }
            
            var newBlackTiles: [Coordinate: Bool] = [:]
            for tile in tilesToCheck.keys {
                var blackNeighborCount = 0
                for neighbor in getNeighbors(tile) {
                    if blackTiles[neighbor] == true {
                        blackNeighborCount += 1
                    }
                }
                if blackTiles[tile] ?? false && (blackNeighborCount == 1 || blackNeighborCount == 2) {
                    newBlackTiles[tile] = true
                } else if !(blackTiles[tile] ?? false) && blackNeighborCount == 2 {
                    newBlackTiles[tile] = true
                }
            }
            
            blackTiles = newBlackTiles
        }
        
        print(blackTiles.count)
    }
}

main()