
import Foundation

struct Coord {
    let x: Double
    let y: Double
    let z: Double
}

struct Point {
    let pos: Coord
    let vel: Coord
}

func parseInput(input: [String]) -> [Point] {
    return input.map { line in
        let components = line.replacingOccurrences(of: " ", with: "").components(separatedBy: "@")
        let posComponents = components[0].components(separatedBy: ",")
        let velComponents = components[1].components(separatedBy: ",")
        
        let pos = Coord(x: Double(posComponents[0])!, y: Double(posComponents[1])!, z: Double(posComponents[2])!)
        let vel = Coord(x: Double(velComponents[0])!, y: Double(velComponents[1])!, z: Double(velComponents[2])!)
        
        return Point(pos: pos, vel: vel)
    }
}

func isIntersecting2D(p1: Point, p2: Point) -> (Bool, Coord, Double, Double) {
    let det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
    if det == 0 {
        return (false, Coord(x: 0, y: 0, z: 0), 0, 0)
    }
    let t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
    let t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
    let coord = Coord(x: p1.pos.x + p1.vel.x * t1, y: p1.pos.y + p1.vel.y * t1, z: 0)
    return (true, coord, t1, t2)
}

func solve(input: [String], min: Double, max: Double) -> Int {
    let points = parseInput(input: input)
    var cnt = 0
    
    for i in 0..<points.count {
        for j in 0..<i {
            let (isIntersecting, coord, time1, time2) = isIntersecting2D(p1: points[i], p2: points[j])
            let isInBound = min <= coord.x && coord.x <= max && min <= coord.y && coord.y <= max
            if isIntersecting && isInBound && time1 >= 0 && time2 >= 0 {
                cnt += 1
            }
        }
    }
    return cnt
}

func readFile(fileName: String) -> [String] {
    do {
        let fileContent = try String(contentsOfFile: fileName)
        return fileContent.components(separatedBy: "\n").filter { !$0.isEmpty }
    } catch {
        fatalError("Error reading file: \(error)")
    }
}

let input = readFile(fileName: "input.txt")
print(solve(input: input, min: 200000000000000, max: 400000000000000))
