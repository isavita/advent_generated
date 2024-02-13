import Foundation

struct Coord {
    var x: Int
    var y: Int
    
    func add(_ other: Coord) -> Coord {
        return Coord(x: self.x + other.x, y: self.y + other.y)
    }
    
    func multiplyByScalar(_ scalar: Int) -> Coord {
        return Coord(x: self.x * scalar, y: self.y * scalar)
    }
}

let north = Coord(x: 0, y: -1)
let west = Coord(x: -1, y: 0)
let south = Coord(x: 0, y: 1)
let east = Coord(x: 1, y: 0)

func abs(_ x: Int) -> Int {
    return x < 0 ? -x : x
}

func parseInput(_ input: [String]) -> [Coord] {
    let up = Character("3")
    let left = Character("2")
    let down = Character("1")
    let right = Character("0")
    
    var current = Coord(x: 0, y: 0)
    var vertices = [current]
    
    for line in input {
        let parts = line.components(separatedBy: " ")
        let color = parts[2]
        let dirInput = color[color.index(color.startIndex, offsetBy: 7)]
        let lengthStr = String(color[color.index(color.startIndex, offsetBy: 2)..<color.index(color.startIndex, offsetBy: 7)])
        let length = Int(lengthStr, radix: 16)!
        
        var dir: Coord
        switch dirInput {
        case up:
            dir = north
        case left:
            dir = west
        case down:
            dir = south
        case right:
            dir = east
        default:
            fatalError("Invalid direction input")
        }
        
        current = current.add(dir.multiplyByScalar(length))
        vertices.append(current)
    }
    
    return vertices
}

func shoelace(_ vertices: [Coord]) -> Int {
    let n = vertices.count
    var area = 0
    
    for i in 0..<n {
        let next = (i + 1) % n
        area += vertices[i].x * vertices[next].y
        area -= vertices[i].y * vertices[next].x
    }
    
    area = abs(area) / 2
    return area
}

func perimeter(_ vertices: [Coord]) -> Int {
    let n = vertices.count
    var perim = 0
    
    for i in 0..<n {
        let next = (i + 1) % n
        perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)
    }
    
    return perim
}

func calculatePolygonArea(_ vertices: [Coord]) -> Int {
    return shoelace(vertices) + perimeter(vertices) / 2 + 1
}

func solve(_ input: [String]) -> Int {
    let vertices = parseInput(input)
    
    let res = calculatePolygonArea(vertices)
    return res
}

func readFile(_ fileName: String) -> [String] {
    let fileURL = URL(fileURLWithPath: fileName)
    let content = try! String(contentsOf: fileURL)
    return content.components(separatedBy: "\n")
}

let input = readFile("input.txt")
print(solve(input))