
import Foundation

func mod(_ a: Int, _ b: Int) -> Int {
    return (a % b + b) % b
}

func parseLine(_ line: String) -> (Int, Int, Int, Int)? {
    let pattern = #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)"#
    guard let regex = try? NSRegularExpression(pattern: pattern, options: []) else { return nil }
    let nsrange = NSRange(line.startIndex..<line.endIndex, in: line)

    if let match = regex.firstMatch(in: line, options: [], range: nsrange) {
        let x = Int((line as NSString).substring(with: match.range(at: 1)))!
        let y = Int((line as NSString).substring(with: match.range(at: 2)))!
        let vx = Int((line as NSString).substring(with: match.range(at: 3)))!
        let vy = Int((line as NSString).substring(with: match.range(at: 4)))!
        return (x, y, vx, vy)
    }
    return nil
}

func moveRobots(robots: [(Int, Int, Int, Int)], sizeX: Int, sizeY: Int) -> [(Int, Int, Int, Int)] {
    return robots.map { (x, y, vx, vy) in
        (mod(x + vx, sizeX), mod(y + vy, sizeY), vx, vy)
    }
}

func countQuadrants(robots: [(Int, Int, Int, Int)], sizeX: Int, sizeY: Int) -> [Int] {
    let centerX = sizeX / 2
    let centerY = sizeY / 2
    var counts = [0, 0, 0, 0]
    for (x, y, _, _) in robots {
        if x < centerX {
            if y < centerY {
                counts[0] += 1
            } else if y > centerY {
                counts[1] += 1
            }
        } else if x > centerX {
            if y < centerY {
                counts[2] += 1
            } else if y > centerY {
                counts[3] += 1
            }
        }
    }
    return counts
}

func hasNoOverlaps(robots: [(Int, Int, Int, Int)]) -> Bool {
    var positions = Set<[Int]>()
    for (x, y, _, _) in robots {
        if !positions.insert([x, y]).inserted {
            return false
        }
    }
    return true
}

func drawGrid(robots: [(Int, Int, Int, Int)], sizeX: Int, sizeY: Int) {
    var grid = [[Character]](repeating: [Character](repeating: ".", count: sizeX), count: sizeY)
    for (x, y, _, _) in robots {
        grid[y][x] = "#"
    }
    for row in grid {
        print(String(row))
    }
}

func main() {
    let sizeX = 101
    let sizeY = 103
    var robots: [(Int, Int, Int, Int)] = []

    do {
        let data = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = data.components(separatedBy: .newlines)
        for line in lines {
            if !line.isEmpty, let robot = parseLine(line) {
                robots.append(robot)
            }
        }
    } catch {
        print("Error reading file: \(error)")
        return
    }

    var robotsPart1 = robots
    for _ in 0..<100 {
        robotsPart1 = moveRobots(robots: robotsPart1, sizeX: sizeX, sizeY: sizeY)
    }
    let counts = countQuadrants(robots: robotsPart1, sizeX: sizeX, sizeY: sizeY)
    let safetyFactor = counts.reduce(1, *)
    print("Part 1 - Safety Factor after 100 seconds: \(safetyFactor)")

    var robotsPart2 = robots
    var seconds = 0
    while true {
        if hasNoOverlaps(robots: robotsPart2) {
            break
        }
        robotsPart2 = moveRobots(robots: robotsPart2, sizeX: sizeX, sizeY: sizeY)
        seconds += 1
        if seconds > 1000000 {
            print("Exceeded maximum iterations without finding a unique position configuration.")
            return
        }
    }
    print("Part 2 - Fewest seconds to display Easter egg: \(seconds)")
    print("Final positions of robots:")
    drawGrid(robots: robotsPart2, sizeX: sizeX, sizeY: sizeY)
}

main()
