
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
}

enum FoldInstruction {
    case horizontal(y: Int)
    case vertical(x: Int)
}

func parseInput(from file: String) -> (Set<Point>, [FoldInstruction])? {
    guard let content = try? String(contentsOfFile: file) else {
        return nil
    }

    let parts = content.components(separatedBy: "\n\n")
    guard parts.count == 2 else { return nil }

    let pointsStrings = parts[0].components(separatedBy: .newlines).filter { !$0.isEmpty }
    let foldStrings = parts[1].components(separatedBy: .newlines).filter { !$0.isEmpty }

    var points = Set<Point>()
    for pointString in pointsStrings {
        let coords = pointString.components(separatedBy: ",").compactMap { Int($0) }
        guard coords.count == 2 else { return nil }
        points.insert(Point(x: coords[0], y: coords[1]))
    }

    var foldInstructions: [FoldInstruction] = []
    for foldString in foldStrings {
        let instructionParts = foldString.components(separatedBy: "=")
        guard instructionParts.count == 2, let value = Int(instructionParts[1]) else { return nil }
        if instructionParts[0].hasSuffix("y") {
            foldInstructions.append(.horizontal(y: value))
        } else if instructionParts[0].hasSuffix("x") {
            foldInstructions.append(.vertical(x: value))
        } else {
            return nil
        }
    }

    return (points, foldInstructions)
}

func fold(points: Set<Point>, instruction: FoldInstruction) -> Set<Point> {
    var foldedPoints = Set<Point>()

    for point in points {
        switch instruction {
        case .horizontal(let y):
            if point.y < y {
                foldedPoints.insert(point)
            } else if point.y > y {
                foldedPoints.insert(Point(x: point.x, y: 2 * y - point.y))
            }
        case .vertical(let x):
            if point.x < x {
                foldedPoints.insert(point)
            } else if point.x > x {
                foldedPoints.insert(Point(x: 2 * x - point.x, y: point.y))
            }
        }
    }
    return foldedPoints
}

func solve() {
    guard let (points, foldInstructions) = parseInput(from: "input.txt"),
          let firstInstruction = foldInstructions.first else {
        print("Error parsing input or no fold instructions.")
        return
    }

    let foldedPoints = fold(points: points, instruction: firstInstruction)
    print(foldedPoints.count)
}

solve()
