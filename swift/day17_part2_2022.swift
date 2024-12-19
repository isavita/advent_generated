
import Foundation

struct Point: Hashable {
    var x: Int
    var y: Int
}

enum Direction {
    case north, east, south, west
    
    func point() -> Point {
        switch self {
        case .north: return Point(x: 0, y: 1)
        case .east: return Point(x: 1, y: 0)
        case .south: return Point(x: 0, y: -1)
        case .west: return Point(x: -1, y: 0)
        }
    }
}

func directionFromByte(_ byte: Character) -> Direction {
    switch byte {
    case ">": return .east
    case "<": return .west
    default: fatalError()
    }
}

let rockstr = """
####

 #
###
 #

  #
  #
###

#
#
#
#

##
##
"""

func getRocks() -> [[Point]] {
    var rocks: [[Point]] = []
    for rock in rockstr.components(separatedBy: "\n\n") {
        var points: [Point] = []
        let lines = rock.components(separatedBy: "\n")
        for (y, line) in lines.enumerated() {
            for (x, char) in line.enumerated() {
                if char == "#" {
                    points.append(Point(x: x, y: lines.count - 1 - y))
                }
            }
        }
        rocks.append(points)
    }
    return rocks
}

func collision(grid: Set<Point>, rock: [Point], pos: Point) -> Bool {
    for p in rock {
        let newP = Point(x: p.x + pos.x, y: p.y + pos.y)
        if grid.contains(newP) || newP.x < 0 || newP.x > 6 {
            return true
        }
    }
    return false
}

let jetPattern = try! String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let rocks = getRocks()
var grid: Set<Point> = []
for x in 0..<7 {
    grid.insert(Point(x: x, y: 0))
}
var floor = 0
var j = 0
var repeatDict: [[Int]: [Int]] = [:]
var i = 0
var curr = 0

while true {
    let key = [curr, j]
    if let r = repeatDict[key] {
        let previ = r[0]
        let prevFloor = r[1]
        if (1000000000000 - i) % (i - previ) == 0 {
            print(floor + (1000000000000 - i) / (i - previ) * (floor - prevFloor))
            break
        }
    }
    repeatDict[key] = [i, floor]
    var currRock = rocks[curr]
    var pos = Point(x: 2, y: floor + 4)
    
    while true {
        let jet = jetPattern[jetPattern.index(jetPattern.startIndex, offsetBy: j)]
        j = (j + 1) % jetPattern.count
        let dir = directionFromByte(jet)
        let newPos = Point(x: pos.x + dir.point().x, y: pos.y + dir.point().y)
        if !collision(grid: grid, rock: currRock, pos: newPos) {
            pos = newPos
        }
        
        let downPos = Point(x: pos.x, y: pos.y - 1)
        if collision(grid: grid, rock: currRock, pos: downPos) {
            for p in currRock {
                let newP = Point(x: p.x + pos.x, y: p.y + pos.y)
                grid.insert(newP)
                if newP.y > floor {
                    floor = newP.y
                }
            }
            break
        }
        pos = downPos
    }
    
    i += 1
    curr = (curr + 1) % rocks.count
}
