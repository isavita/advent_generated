
import Foundation

struct Coord: Hashable {
    let x: Int
    let y: Int

    func add(_ other: Coord) -> Coord {
        return Coord(x: self.x + other.x, y: self.y + other.y)
    }

    func isInBounds(gridWidth: Int, gridHeight: Int) -> Bool {
        return 0 <= self.x && self.x < gridWidth && 0 <= self.y && self.y < gridHeight
    }
}

class Grid {
    let width: Int
    let height: Int
    var data: [Coord: Character]

    init(inputData: [String]) {
        self.width = inputData[0].count
        self.height = inputData.count
        self.data = [:]
        for (y, line) in inputData.enumerated() {
            for (x, char) in line.enumerated() {
                if char != "." {
                    self.data[Coord(x: x, y: y)] = char
                }
            }
        }
    }
}

func shiftSingleRock(grid: Grid, coord: Coord, direction: Coord) {
    guard grid.data[coord] == "O" else { return }

    var current = coord
    var before = coord.add(direction)

    while before.isInBounds(gridWidth: grid.width, gridHeight: grid.height) && grid.data[before] == nil {
        grid.data[before] = "O"
        grid.data[current] = nil
        current = before
        before = before.add(direction)
    }
}

func shiftRocks(grid: Grid, direction: Coord) {
    if direction == Coord(x: 0, y: -1) || direction == Coord(x: -1, y: 0) {
        for x in 0..<grid.width {
            for y in 0..<grid.height {
                shiftSingleRock(grid: grid, coord: Coord(x: x, y: y), direction: direction)
            }
        }
    } else {
        for x in (0..<grid.width).reversed() {
            for y in (0..<grid.height).reversed() {
                shiftSingleRock(grid: grid, coord: Coord(x: x, y: y), direction: direction)
            }
        }
    }
}

func cycleRocks(grid: Grid) {
    shiftRocks(grid: grid, direction: Coord(x: 0, y: -1))
    shiftRocks(grid: grid, direction: Coord(x: -1, y: 0))
    shiftRocks(grid: grid, direction: Coord(x: 0, y: 1))
    shiftRocks(grid: grid, direction: Coord(x: 1, y: 0))
}

func calculateGridKey(grid: Grid) -> Int {
    var key = 0
    for (coord, char) in grid.data {
        if char == "O" {
            key += coord.x + coord.y * grid.width
        }
    }
    return key
}

func calculateLoad(grid: Grid) -> Int {
    var load = 0
    for (coord, char) in grid.data {
        if char == "O" {
            load += grid.height - coord.y
        }
    }
    return load
}

func solve(inputData: [String]) -> Int {
    let numCycles = 1000000000
    let grid = Grid(inputData: inputData)
    var cache: [Int: Int] = [:]

    for i in 0..<numCycles {
        let gridKey = calculateGridKey(grid: grid)
        if let iStartCycle = cache[gridKey] {
            let cycleLength = i - iStartCycle
            let remainingCycles = (numCycles - iStartCycle) % cycleLength
            for _ in 0..<remainingCycles {
                cycleRocks(grid: grid)
            }
            return calculateLoad(grid: grid)
        }
        cache[gridKey] = i
        cycleRocks(grid: grid)
    }

    return calculateLoad(grid: grid)
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

let inputData = readFile(fileName: "input.txt")
let result = solve(inputData: inputData)
print(result)
