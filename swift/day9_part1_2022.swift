import Foundation

struct Point: Hashable {
    var x: Int
    var y: Int

    func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }

    static func == (lhs: Point, rhs: Point) -> Bool {
        return lhs.x == rhs.x && lhs.y == rhs.y
    }
}

var head = Point(x: 0, y: 0)
var tail = Point(x: 0, y: 0)
var visited: Set<Point> = [tail]

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    let lines = fileContent.components(separatedBy: "\n")

    for line in lines {
        let components = line.components(separatedBy: " ")
        let dir = components[0]
        let steps = Int(components[1])!

        for _ in 1...steps {
            switch dir {
            case "R":
                head.x += 1
            case "L":
                head.x -= 1
            case "U":
                head.y += 1
            case "D":
                head.y -= 1
            default:
                break
            }

            if abs(head.x - tail.x) > 1 || abs(head.y - tail.y) > 1 {
                if head.x != tail.x && head.y != tail.y {
                    if head.x > tail.x {
                        tail.x += 1
                    } else {
                        tail.x -= 1
                    }
                    if head.y > tail.y {
                        tail.y += 1
                    } else {
                        tail.y -= 1
                    }
                } else {
                    if head.x > tail.x {
                        tail.x += 1
                    } else if head.x < tail.x {
                        tail.x -= 1
                    }
                    if head.y > tail.y {
                        tail.y += 1
                    } else if head.y < tail.y {
                        tail.y -= 1
                    }
                }
            }

            visited.insert(tail)
        }
    }
} catch {
    print("Error reading file")
}

print(visited.count)