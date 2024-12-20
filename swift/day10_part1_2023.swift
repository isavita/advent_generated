
import Foundation

struct Coord: Hashable {
    let x: Int
    let y: Int
    
    func add(_ other: Coord) -> Coord {
        return Coord(x: x + other.x, y: y + other.y)
    }
    
    func subtract(_ other: Coord) -> Coord {
        return Coord(x: x - other.x, y: y - other.y)
    }
    
    func opposite() -> Coord {
        return Coord(x: -x, y: -y)
    }
}

enum Tile: Character {
    case empty = "."
    case start = "S"
    case vertical = "|"
    case horizontal = "-"
    case topLeftCorner = "J"
    case topRightCorner = "L"
    case bottomLeftCorner = "7"
    case bottomRightCorner = "F"
    case enclosed = "X"
}

typealias Pipe = Set<Coord>

struct Grid {
    let width: Int
    let height: Int
    var data: [Coord: Tile]
}

let undefined = Coord(x: 0, y: 0)
let top = Coord(x: 0, y: -1)
let right = Coord(x: 1, y: 0)
let bottom = Coord(x: 0, y: 1)
let left = Coord(x: -1, y: 0)

let verticalPipe: Pipe = [top, bottom]
let horizontalPipe: Pipe = [left, right]
let topLeftCornerPipe: Pipe = [top, left]
let topRightCornerPipe: Pipe = [top, right]
let bottomLeftCornerPipe: Pipe = [bottom, left]
let bottomRightCornerPipe: Pipe = [bottom, right]

let tileToPipe: [Tile: Pipe] = [
    .vertical: verticalPipe,
    .horizontal: horizontalPipe,
    .topLeftCorner: topLeftCornerPipe,
    .topRightCorner: topRightCornerPipe,
    .bottomLeftCorner: bottomLeftCornerPipe,
    .bottomRightCorner: bottomRightCornerPipe
]

func getPipeFromTile(_ tile: Tile) -> Pipe {
    return tileToPipe[tile] ?? []
}

func getTileFromPipe(_ pipe: Pipe) -> Tile {
    for (tile, associatedPipe) in tileToPipe {
        if pipe == associatedPipe {
            return tile
        }
    }
    return .empty
}

func buildGrid(_ input: [String]) -> Grid {
    var data: [Coord: Tile] = [:]
    for (y, line) in input.enumerated() {
        for (x, char) in line.enumerated() {
            if let tile = Tile(rawValue: char), tile != .empty {
                data[Coord(x: x, y: y)] = tile
            }
        }
    }
    return Grid(width: input[0].count, height: input.count, data: data)
}

func findStart(_ grid: Grid) -> Coord {
    for (coord, value) in grid.data {
        if value == .start {
            return coord
        }
    }
    return Coord(x: 0, y: 0)
}

extension Coord {
    func getPipeFromNeighbors(_ grid: Grid) -> Pipe {
        var pipe: Pipe = []
        let possibleNeighbors: [Coord: Coord] = [
            top: add(top),
            right: add(right),
            bottom: add(bottom),
            left: add(left)
        ]
        for (dir, neighborCoord) in possibleNeighbors {
            if let neighborPipe = getPipeFromTile(grid.data[neighborCoord] ?? .empty) as Pipe?,
               neighborPipe.contains(dir.opposite()) {
                pipe.insert(dir)
            }
        }
        return pipe
    }
}

func pathFinding(_ start: Coord, _ grid: Grid) -> [Coord] {
    var path: [Coord] = [start]
    let startPipe = start.getPipeFromNeighbors(grid)
    var previousDir = startPipe.first!
    var current = start.add(previousDir)
    
    while current != start {
        path.append(current)
        let currentPipe = getPipeFromTile(grid.data[current]!)
        for dir in currentPipe {
            if dir != previousDir.opposite() {
                previousDir = dir
                current = current.add(dir)
                break
            }
        }
    }
    return path
}

func solve(_ input: [String]) -> Int {
    let grid = buildGrid(input)
    let start = findStart(grid)
    let path = pathFinding(start, grid)
    return path.count / 2
}

func readFile(_ fileName: String) -> [String] {
    do {
        let fileURL = URL(fileURLWithPath: fileName)
        let fileContents = try String(contentsOf: fileURL)
        return fileContents.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

let input = readFile("input.txt")
print(solve(input))
