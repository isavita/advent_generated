
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let fileContents = try String(contentsOf: fileURL)
let lines = fileContents.components(separatedBy: .newlines)

var points = [String: Bool]()
var folds = [[Int]]()
var readingPoints = true

for line in lines {
    if line.isEmpty {
        readingPoints = false
        continue
    }
    if readingPoints {
        let parts = line.components(separatedBy: ",")
        let x = Int(parts[0])!
        let y = Int(parts[1])!
        points["\(x),\(y)"] = true
    } else {
        let parts = line.components(separatedBy: "=")
        let val = Int(parts[1])!
        if parts[0].contains("x") {
            folds.append([val, 0])
        } else {
            folds.append([0, val])
        }
    }
}

for (i, fold) in folds.enumerated() {
    var newPoints = [String: Bool]()
    for point in points.keys {
        let parts = point.components(separatedBy: ",")
        var newX = Int(parts[0])!
        var newY = Int(parts[1])!
        if fold[0] != 0 && newX > fold[0] {
            newX = fold[0] - (newX - fold[0])
        } else if fold[1] != 0 && newY > fold[1] {
            newY = fold[1] - (newY - fold[1])
        }
        newPoints["\(newX),\(newY)"] = true
    }
    points = newPoints
    if i == 0 {
        print("Number of dots visible after first fold: \(points.count)")
    }
}

var maxX = 0
var maxY = 0
for point in points.keys {
    let parts = point.components(separatedBy: ",")
    let x = Int(parts[0])!
    let y = Int(parts[1])!
    if x > maxX {
        maxX = x
    }
    if y > maxY {
        maxY = y
    }
}

var grid = [[Character]](repeating: [Character](repeating: " ", count: maxX+1), count: maxY+1)

for point in points.keys {
    let parts = point.components(separatedBy: ",")
    let x = Int(parts[0])!
    let y = Int(parts[1])!
    grid[y][x] = "#"
}

for row in grid {
    print(String(row))
}
