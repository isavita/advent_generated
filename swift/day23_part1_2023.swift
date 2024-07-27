
import Foundation

struct Coord: Hashable {
    let x: Int
    let y: Int
    
    func add(_ other: Coord) -> Coord {
        return Coord(x: self.x + other.x, y: self.y + other.y)
    }
}

struct Grid {
    let width: Int
    let height: Int
    var data: [Coord: Character]
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
    EastSlopes: East
]

struct Edge: Hashable {
    let start: Coord
    let end: Coord
    let weight: Int
}

struct Graph {
    var vertices: Set<Coord>
    var edges: [Coord: Set<Edge>]
}

func isInBounds(_ grid: Grid, _ coord: Coord) -> Bool {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height
}

func parseInput(_ input: [String]) -> Grid {
    var data = [Coord: Character]()
    for (y, line) in input.enumerated() {
        for (x, char) in line.enumerated() {
            if char != Empty {
                data[Coord(x: x, y: y)] = char
            }
        }
    }
    return Grid(width: input[0].count, height: input.count, data: data)
}

func isValidNeighbor(_ grid: Grid, _ coord: Coord, _ dir: Coord) -> Bool {
    return isInBounds(grid, coord) && grid.data[coord] != Wall
}

func isValidNeighborWithSlopes(_ grid: Grid, _ coord: Coord, _ dir: Coord) -> Bool {
    guard isInBounds(grid, coord) else { return false }
    if let char = grid.data[coord] {
        return char != Wall && SlopeToDir[char] == dir
    }
    return true
}

func neighbors4(_ grid: Grid, _ coord: Coord, _ isValidNeighborFunc: (Grid, Coord, Coord) -> Bool) -> [Coord] {
    let directions = [North, South, West, East]
    return directions.compactMap { dir in
        let neighbor = coord.add(dir)
        return isValidNeighborFunc(grid, neighbor, dir) ? neighbor : nil
    }
}

func getGraph(_ grid: Grid, _ start: Coord, _ end: Coord, _ isValidNeighborFunc: @escaping (Grid, Coord, Coord) -> Bool) -> Graph {
    var graph = Graph(vertices: [start, end], edges: [:])
    
    for y in 0..<grid.height {
        for x in 0..<grid.width {
            let coord = Coord(x: x, y: y)
            if grid.data[coord] == nil && neighbors4(grid, coord, isValidNeighbor).count > 2 {
                graph.vertices.insert(coord)
            }
        }
    }
    
    for vertex in graph.vertices {
        graph.edges[vertex] = getEdgesBFS(grid, vertex, graph.vertices, isValidNeighborFunc)
    }
    
    return graph
}

func getEdgesBFS(_ grid: Grid, _ start: Coord, _ vertices: Set<Coord>, _ isValidNeighborFunc: (Grid, Coord, Coord) -> Bool) -> Set<Edge> {
    var frontier = [start]
    var reached = Set([start])
    var distances = [start: 0]
    var edges = Set<Edge>()
    
    while !frontier.isEmpty {
        let current = frontier.removeFirst()
        
        if vertices.contains(current) && current != start {
            edges.insert(Edge(start: start, end: current, weight: distances[current]!))
            continue
        }
        
        for next in neighbors4(grid, current, isValidNeighborFunc) {
            if !reached.contains(next) {
                frontier.append(next)
                reached.insert(next)
                distances[next] = distances[current]! + 1
            }
        }
    }
    
    return edges
}

func getMaxDistanceDFS(_ grid: Grid, _ graph: Graph, _ current: Coord, _ end: Coord, _ seen: inout Set<Coord>) -> (Bool, Int) {
    if current == end { return (true, 0) }
    
    var maxi = 0
    seen.insert(current)
    
    for edge in graph.edges[current] ?? [] {
        if !seen.contains(edge.end) {
            let (isValid, dist) = getMaxDistanceDFS(grid, graph, edge.end, end, &seen)
            if isValid {
                maxi = max(maxi, dist + edge.weight)
            }
        }
    }
    
    seen.remove(current)
    return maxi == 0 ? (false, 0) : (true, maxi)
}

func solve(_ input: [String]) -> Int {
    let grid = parseInput(input)
    let start = Coord(x: 1, y: 0)
    let end = Coord(x: grid.width - 2, y: grid.height - 1)
    let graph = getGraph(grid, start, end, isValidNeighborWithSlopes)
    var seen = Set<Coord>()
    let (_, maxDist) = getMaxDistanceDFS(grid, graph, start, end, &seen)
    return maxDist
}

func readFile(_ fileName: String) -> [String] {
    return try! String(contentsOfFile: fileName).split(separator: "\n").map(String.init)
}

let input = readFile("input.txt")
print(solve(input))
