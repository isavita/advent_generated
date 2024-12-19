
import Foundation

struct Point: Hashable {
    var x: Int
    var y: Int
}

let rocksStr = """
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
    for rock in rocksStr.components(separatedBy: "\n\n") {
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

func main() {
    let jetPattern = try! String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
    let rocks = getRocks()
    var grid: Set<Point> = []
    for x in 0..<7 {
        grid.insert(Point(x: x, y: 0))
    }
    var floor = 0
    var j = 0
    var repeatDict: [[Int]: [Int]] = [:]

    for i in 0... {
        if i == 2022 {
            print(floor)
            break
        }
        let curr = i % rocks.count
        let key = [curr, j]
        repeatDict[key] = [i, floor]
        var currRock = rocks[curr]
        var pos = Point(x: 2, y: floor + 4)

        while true {
            let jet = jetPattern[jetPattern.index(jetPattern.startIndex, offsetBy: j)]
            j = (j + 1) % jetPattern.count
            let dir = jet == ">" ? Point(x: 1, y: 0) : Point(x: -1, y: 0)
            let newPos = Point(x: pos.x + dir.x, y: pos.y + dir.y)
            if !collision(grid: grid, rock: currRock, pos: newPos) {
                pos = newPos
            }
            let downPos = Point(x: pos.x, y: pos.y - 1)
            if collision(grid: grid, rock: currRock, pos: downPos) {
                for p in currRock {
                    let newP = Point(x: p.x + pos.x, y: p.y + pos.y)
                    grid.insert(newP)
                    floor = max(floor, newP.y)
                }
                break
            }
            pos = downPos
        }
    }
}

main()
