
import Foundation

// Function to parse the input file and return an array of line segments
func parseInput(from fileName: String) -> [(Int, Int, Int, Int)] {
    var lines: [(Int, Int, Int, Int)] = []
    
    do {
        let content = try String(contentsOfFile: fileName)
        let lineStrings = content.split(separator: "\n")
        
        for line in lineStrings {
            let points = line.split(separator: " -> ")
            let start = points[0].split(separator: ",").map { Int($0)! }
            let end = points[1].split(separator: ",").map { Int($0)! }
            lines.append((start[0], start[1], end[0], end[1]))
        }
    } catch {
        print("Error reading file: \(error)")
    }
    
    return lines
}

// Function to get all points covered by a line segment
func getPoints(from line: (Int, Int, Int, Int)) -> [(Int, Int)] {
    var points: [(Int, Int)] = []
    let (x1, y1, x2, y2) = line
    
    if x1 == x2 { // Vertical line
        let range = y1 <= y2 ? y1...y2 : y2...y1
        for y in range {
            points.append((x1, y))
        }
    } else if y1 == y2 { // Horizontal line
        let range = x1 <= x2 ? x1...x2 : x2...x1
        for x in range {
            points.append((x, y1))
        }
    } else { // Diagonal line (45 degrees)
        let xStep = x1 < x2 ? 1 : -1
        let yStep = y1 < y2 ? 1 : -1
        var x = x1
        var y = y1
        
        while x != x2 + xStep && y != y2 + yStep {
            points.append((x, y))
            x += xStep
            y += yStep
        }
    }
    
    return points
}

// Main function to calculate the number of overlapping points
func countOverlaps(from fileName: String) -> Int {
    let lines = parseInput(from: fileName)
    var pointCounts: [String: Int] = [:]
    
    for line in lines {
        let points = getPoints(from: line)
        for point in points {
            let key = "\(point.0),\(point.1)"
            pointCounts[key, default: 0] += 1
        }
    }
    
    // Count points with at least 2 overlaps
    return pointCounts.values.filter { $0 >= 2 }.count
}

// Execute the program
let overlapCount = countOverlaps(from: "input.txt")
print("Number of points where at least two lines overlap: \(overlapCount)")
