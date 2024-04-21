import Foundation

struct Point {
    let x, y: Int
}

let directions: [Point] = [
    Point(x: -1, y: -1), Point(x: 0, y: -1), Point(x: 1, y: -1),
    Point(x: -1, y: 0), Point(x: 1, y: 0),
    Point(x: -1, y: 1), Point(x: 0, y: 1), Point(x: 1, y: 1)
]

var seatingArea: [[Character]] = []

do {
    let fileContent = try String(contentsOfFile: "input.txt")
    seatingArea = fileContent.components(separatedBy: "\n").map { Array($0) }
} catch {
    print("Error reading file: \(error)")
    exit(1)
}

var stabilized = false
while !stabilized {
    (seatingArea, stabilized) = simulateSeatingPartTwo(seatingArea)
}

print(countOccupiedSeats(seatingArea))

func simulateSeatingPartTwo(_ seatingArea: [[Character]]) -> ([[Character]], Bool) {
    let rows = seatingArea.count
    let cols = seatingArea[0].count
    var newSeatingArea = [[Character]](repeating: [Character](repeating: " ", count: cols), count: rows)
    for i in 0..<rows {
        for j in 0..<cols {
            newSeatingArea[i][j] = seatingArea[i][j]
        }
    }
    var stabilized = true
    
    for i in 0..<rows {
        for j in 0..<cols {
            switch seatingArea[i][j] {
            case "L":
                if countVisibleOccupied(seatingArea, i, j) == 0 {
                    newSeatingArea[i][j] = "#"
                    stabilized = false
                }
            case "#":
                if countVisibleOccupied(seatingArea, i, j) >= 5 {
                    newSeatingArea[i][j] = "L"
                    stabilized = false
                }
            default:
                break
            }
        }
    }
    
    return (newSeatingArea, stabilized)
}

func countVisibleOccupied(_ seatingArea: [[Character]], _ row: Int, _ col: Int) -> Int {
    var count = 0
    for dir in directions {
        var r = row + dir.y
        var c = col + dir.x
        while r >= 0 && r < seatingArea.count && c >= 0 && c < seatingArea[0].count {
            if seatingArea[r][c] == "L" {
                break
            }
            if seatingArea[r][c] == "#" {
                count += 1
                break
            }
            r += dir.y
            c += dir.x
        }
    }
    return count
}

func countOccupiedSeats(_ seatingArea: [[Character]]) -> Int {
    var count = 0
    for row in seatingArea {
        for seat in row {
            if seat == "#" {
                count += 1
            }
        }
    }
    return count
}