
import Foundation

struct Coord: Hashable {
    let x: Int
    let y: Int

    func add(_ other: Coord) -> Coord {
        return Coord(x: self.x + other.x, y: self.y + other.y)
    }

    func subtract(_ other: Coord) -> Coord {
        return Coord(x: self.x - other.x, y: self.y - other.y)
    }

    func opposite() -> Coord {
        return Coord(x: -self.x, y: -self.y)
    }
}

typealias Tile = Character
typealias Pipe = [Coord: Set<Coord>]
typealias Grid = [Coord: Tile]

let Undefined = Coord(x: 0, y: 0)
let Top = Coord(x: 0, y: -1)
let Right = Coord(x: 1, y: 0)
let Bottom = Coord(x: 0, y: 1)
let Left = Coord(x: -1, y: 0)

let Empty: Tile = "."
let Start: Tile = "S"
let Vertical: Tile = "|"
let Horizontal: Tile = "-"
let TopLeftCorner: Tile = "J"
let TopRightCorner: Tile = "L"
let BottomLeftCorner: Tile = "7"
let BottomRightCorner: Tile = "F"
let Enclosed: Tile = "X"

let VerticalPipe: Pipe = [Top: [], Bottom: []]
let HorizontalPipe: Pipe = [Left: [], Right: []]
let TopLeftCornerPipe: Pipe = [Top: [], Left: []]
let TopRightCornerPipe: Pipe = [Top: [], Right: []]
let BottomLeftCornerPipe: Pipe = [Bottom: [], Left: []]
let BottomRightCornerPipe: Pipe = [Bottom: [], Right: []]

let TileToPipe: [Tile: Pipe] = [
    Vertical: VerticalPipe,
    Horizontal: HorizontalPipe,
    TopLeftCorner: TopLeftCornerPipe,
    TopRightCorner: TopRightCornerPipe,
    BottomLeftCorner: BottomLeftCornerPipe,
    BottomRightCorner: BottomRightCornerPipe
]

func getPipeFromTile(tile: Tile) -> Pipe {
    return TileToPipe[tile] ?? [:]
}

func getTileFromPipe(pipe: Pipe) -> Tile {
    for (tile, associatedPipe) in TileToPipe {
        if isEqualPipe(pipe1: pipe, pipe2: associatedPipe) {
            return tile
        }
    }
    return Empty
}

func isEqualPipe(pipe1: Pipe, pipe2: Pipe) -> Bool {
    guard pipe1.count == pipe2.count else {
        return false
    }
    for dir in pipe1.keys {
        if pipe2[dir] == nil {
            return false
        }
    }
    return true
}

func buildGrid(inputLines: [String]) -> Grid {
    var grid: Grid = [:]
    for (y, line) in inputLines.enumerated() {
        for (x, char) in line.enumerated() {
            if char != Empty {
                grid[Coord(x: x, y: y)] = char
            }
        }
    }
    return grid
}

func findStart(grid: Grid) -> Coord {
    for (coord, value) in grid {
        if value == Start {
            return coord
        }
    }
    return Coord(x: 0, y: 0)
}

func getPipeFromNeighbors(coord: Coord, grid: Grid) -> Pipe {
    var pipe: Pipe = [:]
    let possibleNeighbors: [Coord: Coord] = [
        Top: coord.add(Top),
        Right: coord.add(Right),
        Bottom: coord.add(Bottom),
        Left: coord.add(Left)
    ]

    for (dir, neighborCoord) in possibleNeighbors {
        if let neighborTile = grid[neighborCoord] {
            let neighborPipe = getPipeFromTile(tile: neighborTile)
            if neighborPipe[dir.opposite()] != nil {
                pipe[dir] = []
            }
        }
    }
    return pipe
}

func pathFinding(start: Coord, grid: Grid) -> [Coord] {
    var path: [Coord] = [start]
    let startPipe = getPipeFromNeighbors(coord: start, grid: grid)
    var previousDir: Coord? = nil
    var current: Coord? = nil

    for dir in startPipe.keys {
        previousDir = dir
        current = start.add(dir)
        break
    }

    while current != start {
        guard let currentCoord = current else { break }
        path.append(currentCoord)
        guard let currentTile = grid[currentCoord] else { break }
        let currentPipe = getPipeFromTile(tile: currentTile)
        
        for dir in currentPipe.keys {
            if dir != previousDir?.opposite() {
                previousDir = dir
                current = currentCoord.add(dir)
                break
            }
        }
        if current == nil { break } // safety net
    }

    return path
}

func getPathGrid(grid: Grid, path: [Coord], empty: Tile) -> Grid {
    var newGrid: Grid = [:]
    for coord in path {
        if let tile = grid[coord] {
            newGrid[coord] = tile
        }
    }
    if let start = path.first {
      newGrid[start] = getTileFromPipe(pipe: getPipeFromNeighbors(coord: start, grid: grid))
    }
    return newGrid
}

func isInside(coord: Coord, grid: Grid, empty: Tile) -> Bool {
    if grid[coord] != nil {
        return false
    }

    var startPipe: Tile = empty
    var numPipeOnLeft = 0

    for x in 0..<coord.x {
        let c = Coord(x: x, y: coord.y)
        if let v = grid[c] {
            if v == Vertical {
                numPipeOnLeft += 1
            } else if v == TopRightCorner {
                startPipe = TopRightCorner
            } else if v == BottomRightCorner {
                startPipe = BottomRightCorner
            } else if v == TopLeftCorner {
                if startPipe == BottomRightCorner {
                    startPipe = empty
                    numPipeOnLeft += 1
                } else if startPipe == TopRightCorner {
                    startPipe = empty
                }
            } else if v == BottomLeftCorner {
                if startPipe == TopRightCorner {
                    startPipe = empty
                    numPipeOnLeft += 1
                } else if startPipe == BottomRightCorner {
                    startPipe = empty
                }
            }
        }
    }
    return numPipeOnLeft % 2 == 1
}

func solve(inputLines: [String]) -> Int {
    let grid = buildGrid(inputLines: inputLines)
    let start = findStart(grid: grid)
    let path = pathFinding(start: start, grid: grid)
    let pathGrid = getPathGrid(grid: grid, path: path, empty: Empty)

    var count = 0
    if let firstLine = inputLines.first {
        for y in 0..<inputLines.count {
            for x in 0..<firstLine.count {
                let c = Coord(x: x, y: y)
                if isInside(coord: c, grid: pathGrid, empty: Empty) {
                    count += 1
                }
            }
        }
    }
    return count
}

func readFile(fileName: String) -> [String] {
    do {
        let contents = try String(contentsOfFile: fileName, encoding: .utf8)
        return contents.components(separatedBy: .newlines).filter { !$0.isEmpty }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

let inputLines = readFile(fileName: "input.txt")
print(solve(inputLines: inputLines))
