
import Foundation

struct Coord: Hashable {
    var x: Int
    var y: Int
}

struct Grid {
    var width: Int
    var height: Int
    var data: [Coord: Character]
}

let empty: Character = "."

func buildGrid(input: [String], empty: Character) -> Grid {
    var data = [Coord: Character]()
    let width = input[0].count
    let height = input.count

    for y in 0..<height {
        for x in 0..<width {
            let char = input[y][input[y].index(input[y].startIndex, offsetBy: x)]
            if char != empty {
                data[Coord(x: x, y: y)] = char
            }
        }
    }

    return Grid(width: width, height: height, data: data)
}

func toString(grid: Grid, empty: Character) -> String {
    var result = ""

    for y in 0..<grid.height {
        for x in 0..<grid.width {
            let coord = Coord(x: x, y: y)
            if let v = grid.data[coord] {
                result.append(v)
            } else {
                result.append(empty)
            }
        }
        result.append("\n")
    }

    return result
}

func getEmptyRows(grid: Grid) -> [Int] {
    var emptyRows = [Int]()
    for y in 0..<grid.height {
        var isEmpty = true

        var x = 0
        while x < grid.width {
            if grid.data[Coord(x: x, y: y)] != nil {
                isEmpty = false
            }
            x += 1
        }

        if isEmpty {
            emptyRows.append(y)
        }
    }
    return emptyRows
}

func getEmptyCols(grid: Grid) -> [Int] {
    var emptyCols = [Int]()
    for x in 0..<grid.width {
        var isEmpty = true

        var y = 0
        while y < grid.height {
            if grid.data[Coord(x: x, y: y)] != nil {
                isEmpty = false
            }
            y += 1
        }

        if isEmpty {
            emptyCols.append(x)
        }
    }
    return emptyCols
}

func calculateOffsets(emptyIndexes: [Int], bound: Int) -> [Int] {
    var offsets = [Int](repeating: 0, count: bound)
    for idx in emptyIndexes {
        for i in idx + 1..<offsets.count {
            offsets[i] += 1
        }
    }
    return offsets
}

func expandGrid(grid: Grid, expansionFactor: Int) -> Grid {
    let emptyCols = getEmptyCols(grid: grid)
    let emptyRows = getEmptyRows(grid: grid)
    let numLinesToAdd = expansionFactor - 1

    var newGrid = Grid(width: grid.width + emptyCols.count*numLinesToAdd, height: grid.height + emptyRows.count*numLinesToAdd, data: [:])

    let dXs = calculateOffsets(emptyIndexes: emptyCols, bound: grid.width)
    let dYs = calculateOffsets(emptyIndexes: emptyRows, bound: grid.height)

    for y in 0..<grid.height {
        for x in 0..<grid.width {
            let coord = Coord(x: x, y: y)
            if let value = grid.data[coord] {
                let newCoord = Coord(x: x + dXs[x]*numLinesToAdd, y: y + dYs[y]*numLinesToAdd)
                newGrid.data[newCoord] = value
            }
        }
    }

    return newGrid
}

func abs(_ x: Int) -> Int {
    return x < 0 ? -x : x
}

func calculateLength(grid: Grid, c1: Coord, c2: Coord) -> Int {
    let dX = abs(c2.x - c1.x)
    let dY = abs(c2.y - c1.y)
    return dX + dY
}

func solve(input: [String], expansionFactor: Int) -> Int {
    let grid = buildGrid(input: input, empty: empty)
    let expandedGrid = expandGrid(grid: grid, expansionFactor: expansionFactor)

    var res = 0
    var alreadySeen = Set<Coord>()
    for coord1 in expandedGrid.data.keys {
        for coord2 in alreadySeen {
            let length = calculateLength(grid: expandedGrid, c1: coord1, c2: coord2)
            res += length
        }
        alreadySeen.insert(coord1)
    }

    return res
}

func readFile(fileName: String) -> [String] {
    let fileURL = URL(fileURLWithPath: fileName)
    do {
        let content = try String(contentsOf: fileURL)
        return content.components(separatedBy: "\n")
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

let input = readFile(fileName: "input.txt")
print(solve(input: input, expansionFactor: 1000000))
