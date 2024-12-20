
import Foundation

struct Coord: Hashable {
    let x: Int
    let y: Int
    
    func add(_ other: Coord) -> Coord {
        return Coord(x: x + other.x, y: y + other.y)
    }
}

struct Grid {
    let width: Int
    let height: Int
    let data: [Coord: Character]
}

let North = Coord(x: 0, y: -1)
let South = Coord(x: 0, y: 1)
let West = Coord(x: -1, y: 0)
let East = Coord(x: 1, y: 0)

let Empty: Character = "."
let Wall: Character = "#"
let NorthSlopes: Character = "^"
let SouthSlopes: Character = "v"
let WestSlopes: Character = "<"
let EastSlopes: Character = ">"

let SlopeToDir: [Character: Coord] = [
    NorthSlopes: North,
    SouthSlopes: South,
    WestSlopes: West,
    EastSlopes: East,
]

struct Edge: Hashable {
    let start: Coord
    let end: Coord
    let weight: Int
}

struct Graph {
    let vertices: Set<Coord>
    let edges: [Coord: Set<Edge>]
}

func isInBounds(grid: Grid, coord: Coord) -> Bool {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height
}

func parseInput(input: [String]) -> Grid {
    var data: [Coord: Character] = [:]
    
    for (y, line) in input.enumerated() {
        for (x, char) in line.enumerated() {
            if char != Empty {
                data[Coord(x: x, y: y)] = char
            }
        }
    }
    
    return Grid(width: input[0].count, height: input.count, data: data)
}

func isValidNeighbor(grid: Grid, coord: Coord, dir: Coord) -> Bool {
    if !isInBounds(grid: grid, coord: coord) {
        return false
    }
    if grid.data[coord] == Wall {
        return false
    }
    return true
}

func neighbors4(grid: Grid, coord: Coord, isValidNeighborFunc: (Grid, Coord, Coord) -> Bool) -> [Coord] {
    let directions = [North, South, West, East]
    var validNeighbors: [Coord] = []
    
    for dir in directions {
        let neighbor = coord.add(dir)
        if isValidNeighborFunc(grid, neighbor, dir) {
            validNeighbors.append(neighbor)
        }
    }
    
    return validNeighbors
}

func getGraph(grid: Grid, start: Coord, end: Coord, isValidNeighborFunc: (Grid, Coord, Coord) -> Bool) -> Graph {
    var vertices: Set<Coord> = [start, end]
    var edges: [Coord: Set<Edge>] = [:]
    
    for y in 0..<grid.height {
        for x in 0..<grid.width {
            let coord = Coord(x: x, y: y)
            if grid.data[coord] == nil {
                if neighbors4(grid: grid, coord: coord, isValidNeighborFunc: isValidNeighbor).count > 2 {
                    vertices.insert(coord)
                }
            }
        }
    }
    
    for startVertex in vertices {
        edges[startVertex] = getEdgesBFS(grid: grid, start: startVertex, vertices: vertices, isValidNeighborFunc: isValidNeighborFunc)
    }
    
    return Graph(vertices: vertices, edges: edges)
}

func getEdgesBFS(grid: Grid, start: Coord, vertices: Set<Coord>, isValidNeighborFunc: (Grid, Coord, Coord) -> Bool) -> Set<Edge> {
    var frontier: [Coord] = [start]
    var reached: Set<Coord> = [start]
    var distances: [Coord: Int] = [start: 0]
    var edges: Set<Edge> = []
    
    while !frontier.isEmpty {
        let current = frontier.removeFirst()
        
        if vertices.contains(current) && current != start {
            edges.insert(Edge(start: start, end: current, weight: distances[current]!))
            continue
        }
        
        for next in neighbors4(grid: grid, coord: current, isValidNeighborFunc: isValidNeighborFunc) {
            if !reached.contains(next) {
                frontier.append(next)
                reached.insert(next)
                distances[next] = distances[current]! + 1
            }
        }
    }
    
    return edges
}

func getMaxDistanceDFS(graph: Graph, current: Coord, end: Coord, seen: inout Set<Coord>) -> (Bool, Int) {
    if current == end {
        return (true, 0)
    }
    
    var maxi = 0
    seen.insert(current)
    
    if let currentEdges = graph.edges[current] {
        for edge in currentEdges {
            if !seen.contains(edge.end) {
                let (isValid, dist) = getMaxDistanceDFS(graph: graph, current: edge.end, end: end, seen: &seen)
                if isValid {
                    maxi = max(maxi, dist + edge.weight)
                }
            }
        }
    }
    
    seen.remove(current)
    
    if maxi == 0 {
        return (false, 0)
    }
    return (true, maxi)
}

func solve(input: [String]) -> Int {
    let grid = parseInput(input: input)
    
    let start = Coord(x: 1, y: 0)
    let end = Coord(x: grid.width - 2, y: grid.height - 1)
    
    let graph = getGraph(grid: grid, start: start, end: end, isValidNeighborFunc: isValidNeighbor)
    
    var seen: Set<Coord> = []
    let (_, maxDist) = getMaxDistanceDFS(graph: graph, current: start, end: end, seen: &seen)
    return maxDist
}

func readFile(fileName: String) -> [String] {
    do {
        let fileURL = URL(fileURLWithPath: fileName)
        let fileContents = try String(contentsOf: fileURL)
        return fileContents.components(separatedBy: "\n").filter { !$0.isEmpty }
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

let input = readFile(fileName: "input.txt")
print(solve(input: input))
