
import Foundation

struct Coord {
    var x: Int
    var y: Int
    var z: Int
}

class Brick {
    var mini: Coord
    var maxi: Coord
    var basedOn: [Brick] = []
    var support: [Brick] = []
    
    init(mini: Coord, maxi: Coord) {
        self.mini = mini
        self.maxi = maxi
    }
}

func parseInput(_ input: [String]) -> [Brick] {
    return input.map { line in
        let components = line.split(separator: "~").flatMap { $0.split(separator: ",").compactMap { Int($0) } }
        return Brick(mini: Coord(x: components[0], y: components[1], z: components[2]),
                     maxi: Coord(x: components[3], y: components[4], z: components[5]))
    }
}

func settle(_ bricks: inout [Brick]) {
    bricks.sort { $0.maxi.z < $1.maxi.z }
    
    for i in 0..<bricks.count {
        var supportZ = 0
        var basedBricks: [Brick] = []
        
        for j in (0..<i).reversed() {
            let isIntersectingX = max(bricks[i].mini.x, bricks[j].mini.x) <= min(bricks[i].maxi.x, bricks[j].maxi.x)
            let isIntersectingY = max(bricks[i].mini.y, bricks[j].mini.y) <= min(bricks[i].maxi.y, bricks[j].maxi.y)
            if isIntersectingX && isIntersectingY {
                if bricks[j].maxi.z == supportZ {
                    basedBricks.append(bricks[j])
                } else if bricks[j].maxi.z > supportZ {
                    supportZ = bricks[j].maxi.z
                    basedBricks = [bricks[j]]
                }
            }
        }
        
        bricks[i].basedOn = basedBricks
        for basedBrick in basedBricks {
            basedBrick.support.append(bricks[i])
        }
        
        let deltaZ = bricks[i].maxi.z - bricks[i].mini.z
        bricks[i].mini.z = supportZ + 1
        bricks[i].maxi.z = bricks[i].mini.z + deltaZ
    }
}

func solve(_ input: [String]) -> Int {
    var bricks = parseInput(input)
    settle(&bricks)
    
    return bricks.reduce(0) { count, brick in
        let isDisintegratable = brick.support.allSatisfy { $0.basedOn.count >= 2 }
        return count + (isDisintegratable ? 1 : 0)
    }
}

func readFile(_ fileName: String) -> [String] {
    guard let content = try? String(contentsOfFile: fileName) else { return [] }
    return content.split(separator: "\n").map(String.init)
}

let input = readFile("input.txt")
print(solve(input))
