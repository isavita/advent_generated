
import Foundation

struct Coord: Hashable {
    let x: Int
    let y: Int
    
    func add(_ other: Coord) -> Coord {
        return Coord(x: x + other.x, y: y + other.y)
    }
    
    func rotate90() -> Coord {
        return Coord(x: y, y: -x)
    }
    
    func rotateNeg90() -> Coord {
        return Coord(x: -y, y: x)
    }
    
    func isInBounds(width: Int, height: Int) -> Bool {
        return 0 <= x && x < width && 0 <= y && y < height
    }
}

struct Beam: Hashable {
    let origin: Coord
    let dir: Coord
}

enum Tile: Character {
    case empty = "."
    case ascendingMirror = "/"
    case descendingMirror = "\\"
    case verticalSplitter = "|"
    case horizontalSplitter = "-"
}

let north = Coord(x: 0, y: -1)
let west = Coord(x: -1, y: 0)
let south = Coord(x: 0, y: 1)
let east = Coord(x: 1, y: 0)

func buildGrid(input: [String]) -> [[Tile?]] {
    return input.map { line in
        line.map { Tile(rawValue: $0) }
    }
}

func nextBeam(grid: [[Tile?]], beam: Beam) -> [Beam] {
    guard let tile = grid[beam.origin.y][beam.origin.x] else {
        return [Beam(origin: beam.origin.add(beam.dir), dir: beam.dir)]
    }
    
    switch tile {
    case .ascendingMirror:
        let newDir = (beam.dir == north || beam.dir == south) ? beam.dir.rotateNeg90() : beam.dir.rotate90()
        return [Beam(origin: beam.origin.add(newDir), dir: newDir)]
        
    case .descendingMirror:
        let newDir = (beam.dir == north || beam.dir == south) ? beam.dir.rotate90() : beam.dir.rotateNeg90()
        return [Beam(origin: beam.origin.add(newDir), dir: newDir)]
        
    case .verticalSplitter where beam.dir == east || beam.dir == west:
        return [
            Beam(origin: beam.origin.add(beam.dir.rotate90()), dir: beam.dir.rotate90()),
            Beam(origin: beam.origin.add(beam.dir.rotateNeg90()), dir: beam.dir.rotateNeg90())
        ]
        
    case .horizontalSplitter where beam.dir == north || beam.dir == south:
        return [
            Beam(origin: beam.origin.add(beam.dir.rotate90()), dir: beam.dir.rotate90()),
            Beam(origin: beam.origin.add(beam.dir.rotateNeg90()), dir: beam.dir.rotateNeg90())
        ]
        
    default:
        return [Beam(origin: beam.origin.add(beam.dir), dir: beam.dir)]
    }
}

func calculatePropagation(grid: [[Tile?]], start: Beam) -> Set<Beam> {
    var alreadySeen: Set<Beam> = []
    var toExplore: [Beam] = [start]
    
    while !toExplore.isEmpty {
        let beam = toExplore.removeFirst()
        
        if beam.origin.isInBounds(width: grid[0].count, height: grid.count) && !alreadySeen.contains(beam) {
            alreadySeen.insert(beam)
            toExplore.append(contentsOf: nextBeam(grid: grid, beam: beam))
        }
    }
    
    return alreadySeen
}

func calculateEnergization(alreadySeen: Set<Beam>) -> Set<Coord> {
    return Set(alreadySeen.map { $0.origin })
}

func solve(input: [String]) -> Int {
    let grid = buildGrid(input: input)
    let start = Beam(origin: Coord(x: 0, y: 0), dir: east)
    
    let alreadySeen = calculatePropagation(grid: grid, start: start)
    let alreadyEnergized = calculateEnergization(alreadySeen: alreadySeen)
    
    return alreadyEnergized.count
}

let input = try! String(contentsOfFile: "input.txt").split(separator: "\n").map { String($0) }
print(solve(input: input))
