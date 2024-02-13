
import Foundation

struct Coord: Hashable {
    var x: Int
    var y: Int
    
    func add(_ other: Coord) -> Coord {
        return Coord(x: self.x + other.x, y: self.y + other.y)
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }
    
    static func == (lhs: Coord, rhs: Coord) -> Bool {
        return lhs.x == rhs.x && lhs.y == rhs.y
    }
}

struct Grid {
    var width: Int
    var height: Int
    var data: [Coord: Character]
}

let north = Coord(x: 0, y: -1)
let west = Coord(x: -1, y: 0)
let south = Coord(x: 0, y: 1)
let east = Coord(x: 1, y: 0)

let empty: Character = "."
let rock: Character = "#"
let start: Character = "S"

func isInBounds(grid: Grid, coord: Coord) -> Bool {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height
}

func parseInput(input: [String]) -> Grid {
    var grid = Grid(width: input[0].count, height: input.count, data: [:])
    
    for (y, line) in input.enumerated() {
        for (x, char) in line.enumerated() {
            if char != empty {
                grid.data[Coord(x: x, y: y)] = char
            }
        }
    }
    
    return grid
}

func findStart(grid: Grid) -> Coord {
    for (coord, char) in grid.data {
        if char == start {
            return coord
        }
    }
    fatalError("No start found.")
}

func neighbors4(grid: Grid, coord: Coord) -> [Coord] {
    let neighbors = [
        coord.add(north),
        coord.add(south),
        coord.add(east),
        coord.add(west)
    ]
    
    var validNeighbors: [Coord] = []
    
    for neighbor in neighbors {
        if isInBounds(grid: grid, coord: neighbor) && grid.data[neighbor] != rock {
            validNeighbors.append(neighbor)
        }
    }
    
    return validNeighbors
}

func breadthFirstSearch(grid: Grid, start: Coord, neighborFunc: (Grid, Coord) -> [Coord]) -> [Coord: Int] {
    var frontier = [start]
    var reached: [Coord: Bool] = [start: true]
    var cameFrom: [Coord: Coord] = [start: start]
    var distances: [Coord: Int] = [start: 0]
    
    while !frontier.isEmpty {
        let current = frontier.removeFirst()
        
        for next in neighborFunc(grid, current) {
            if reached[next] == nil {
                frontier.append(next)
                reached[next] = true
                cameFrom[next] = current
                distances[next] = distances[current]! + 1
            }
        }
    }
    
    return distances
}

func solve(input: [String], numSteps: Int) -> Int {
    let grid = parseInput(input: input)
    
    let start = findStart(grid: grid)
    let distances = breadthFirstSearch(grid: grid, start: start, neighborFunc: neighbors4)
    
    var cnt = 0
    for (_, dist) in distances {
        if dist <= numSteps && dist % 2 == 0 {
            cnt += 1
        }
    }
    return cnt
}

func readFile(fileName: String) -> [String] {
    do {
        let contents = try String(contentsOfFile: fileName)
        return contents.components(separatedBy: "\n")
    } catch {
        fatalError("Error reading file")
    }
}

let input = readFile(fileName: "input.txt")
print(solve(input: input, numSteps: 64))
