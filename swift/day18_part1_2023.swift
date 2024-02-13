
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
    var current = Coord(x: 0, y: 0)
    var vertices = [current]
    
    for line in input {
        let parts = line.components(separatedBy: " ")
        let dirInput = parts[0].first!
        let lengthStr = parts[1]
        var length = 0
        for char in lengthStr {
            length = length * 10 + Int(char.asciiValue! - 48)
        }
        
        var dir: Coord
        switch dirInput {
        case "U":
            dir = north
        case "L":
            dir = west
        case "D":
            dir = south
        case "R":
            dir = east
        default:
            fatalError("Invalid direction")
        }
        
        current = current.add(dir.multiplyByScalar(length))
        vertices.append(current)
    }
    
    return vertices
}

func hexStringToInt(_ hexStr: String) -> Int {
    return Int(hexStr, radix: 16)!
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
    let fileContents = try! String(contentsOf: fileURL)
    return fileContents.components(separatedBy: "\n")
}

let input = readFile("input.txt")
print(solve(input))
