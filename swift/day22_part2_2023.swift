
import Foundation

struct Coord {
    var x, y, z: Int
}

class Brick: Equatable, Hashable {
    var mini, maxi: Coord
    var basedOn: [Brick] = []
    var support: [Brick] = []

    init(mini: Coord, maxi: Coord) {
        self.mini = mini
        self.maxi = maxi
    }

    static func == (lhs: Brick, rhs: Brick) -> Bool {
        return lhs.mini.x == rhs.mini.x && lhs.mini.y == rhs.mini.y && lhs.mini.z == rhs.mini.z &&
               lhs.maxi.x == rhs.maxi.x && lhs.maxi.y == rhs.maxi.y && lhs.maxi.z == rhs.maxi.z
    }
    
    func hash(into hasher: inout Hasher) {
            hasher.combine(mini.x)
            hasher.combine(mini.y)
            hasher.combine(mini.z)
            hasher.combine(maxi.x)
            hasher.combine(maxi.y)
            hasher.combine(maxi.z)
        }
}

func parseInput(_ input: [String]) -> [Brick] {
    return input.map { line in
        let parts = line.components(separatedBy: "~")
        let p1 = parts[0].split(separator: ",").map { Int($0)! }
        let p2 = parts[1].split(separator: ",").map { Int($0)! }
        return Brick(mini: Coord(x: p1[0], y: p1[1], z: p1[2]), maxi: Coord(x: p2[0], y: p2[1], z: p2[2]))
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

    var cnt = 0
    for brick in bricks {
        var fallingBricks = Set<Brick>()
        var queue = brick.support.filter { $0.basedOn.count == 1 }
        
        for supportedBrick in queue {
            if supportedBrick.basedOn.contains(brick){
                 fallingBricks.insert(supportedBrick)
            }
        }

        while !queue.isEmpty {
            let supportedBrick0 = queue.removeFirst()
            
            var isFalling = true
            for basedBrick in supportedBrick0.basedOn {
                if basedBrick != brick && !fallingBricks.contains(basedBrick) {
                    isFalling = false
                    break
                }
            }
            
            if isFalling {
                fallingBricks.insert(supportedBrick0)
                
                let next = supportedBrick0.support.filter{ s in
                    !fallingBricks.contains(s) && s.basedOn.allSatisfy{fallingBricks.contains($0) || $0 == brick }
                }
                queue.append(contentsOf: next)
            }
        }
    
        cnt += fallingBricks.count
    }

    return cnt
}

func readFile(_ fileName: String) -> [String] {
    let fileURL = URL(fileURLWithPath: fileName)
    guard let content = try? String(contentsOf: fileURL) else {
        return []
    }
    return content.components(separatedBy: .newlines).filter { !$0.isEmpty }
}

func main() {
    let input = readFile("input.txt")
    print(solve(input))
}

main()
