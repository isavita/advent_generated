
import Foundation

struct Nanobot {
    var x: Int
    var y: Int
    var z: Int
    var radius: Int
}

func parseNanobots(_ input: String) -> [Nanobot] {
    let lines = input.components(separatedBy: .newlines)
    let pattern = #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)"#
    let regex = try! NSRegularExpression(pattern: pattern)
    
    return lines.compactMap { line in
        let nsRange = NSRange(line.startIndex..<line.endIndex, in: line)
        if let match = regex.firstMatch(in: line, range: nsRange) {
            let x = Int((line as NSString).substring(with: match.range(at: 1)))!
            let y = Int((line as NSString).substring(with: match.range(at: 2)))!
            let z = Int((line as NSString).substring(with: match.range(at: 3)))!
            let radius = Int((line as NSString).substring(with: match.range(at: 4)))!
            return Nanobot(x: x, y: y, z: z, radius: radius)
        }
        return nil
    }
}

func findStrongestNanobot(_ nanobots: [Nanobot]) -> Nanobot {
    return nanobots.max { $0.radius < $1.radius }!
}

func countNanobotsInRange(_ nanobots: [Nanobot], _ strongest: Nanobot) -> Int {
    return nanobots.reduce(0) { result, nanobot in
        return result + (manhattanDistance(nanobot, strongest) <= strongest.radius ? 1 : 0)
    }
}

func manhattanDistance(_ a: Nanobot, _ b: Nanobot) -> Int {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)
}

if let input = try? String(contentsOfFile: "input.txt") {
    let nanobots = parseNanobots(input)
    let strongest = findStrongestNanobot(nanobots)
    let inRangeCount = countNanobotsInRange(nanobots, strongest)
    print(inRangeCount)
}
