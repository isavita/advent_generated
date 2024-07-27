
import Foundation

struct Star {
    var x: Int
    var y: Int
    var vX: Int
    var vY: Int
}

func readInput() -> [Star] {
    let input = try! String(contentsOfFile: "input.txt")
    let regex = try! NSRegularExpression(pattern: "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>")
    var stars = [Star]()
    
    for line in input.split(separator: "\n") {
        let nsLine = NSString(string: String(line))
        let matches = regex.matches(in: String(line), range: NSRange(location: 0, length: nsLine.length))
        for match in matches {
            let x = Int(nsLine.substring(with: match.range(at: 1)))!
            let y = Int(nsLine.substring(with: match.range(at: 2)))!
            let vX = Int(nsLine.substring(with: match.range(at: 3)))!
            let vY = Int(nsLine.substring(with: match.range(at: 4)))!
            stars.append(Star(x: x, y: y, vX: vX, vY: vY))
        }
    }
    return stars
}

func findSmallestAreaTime(stars: [Star]) -> Int {
    var smallestArea = Int.max
    var smallestT = 0
    
    for t in 1..<100000 {
        var minX = Int.max, maxX = Int.min, minY = Int.max, maxY = Int.min
        
        for star in stars {
            let x = star.x + star.vX * t
            let y = star.y + star.vY * t
            minX = min(minX, x)
            maxX = max(maxX, x)
            minY = min(minY, y)
            maxY = max(maxY, y)
        }
        
        let area = (maxX - minX + 1) * (maxY - minY + 1)
        
        if area < smallestArea {
            smallestArea = area
            smallestT = t
        }
    }
    return smallestT
}

func printStars(stars: [Star], time: Int) {
    var positions = [(Int, Int)]()
    
    for star in stars {
        let x = star.x + star.vX * time
        let y = star.y + star.vY * time
        positions.append((x, y))
    }
    
    let minX = positions.map { $0.0 }.min()!
    let maxX = positions.map { $0.0 }.max()!
    let minY = positions.map { $0.1 }.min()!
    let maxY = positions.map { $0.1 }.max()!
    
    var grid = Array(repeating: Array(repeating: " ", count: maxX - minX + 1), count: maxY - minY + 1)
    
    for (x, y) in positions {
        grid[y - minY][x - minX] = "#"
    }
    
    for row in grid {
        print(row.joined())
    }
}

let stars = readInput()
let smallestT = findSmallestAreaTime(stars: stars)
printStars(stars: stars, time: smallestT)
