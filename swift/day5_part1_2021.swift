
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var points: [String: Int] = [:]

for line in lines {
    let coordinates = line.components(separatedBy: " -> ")
    let start = coordinates[0].components(separatedBy: ",")
    let end = coordinates[1].components(separatedBy: ",")
    
    if start[0] == end[0] {
        let y1 = Int(start[1])!
        let y2 = Int(end[1])!
        
        for y in min(y1, y2)...max(y1, y2) {
            let key = "\(start[0]),\(y)"
            points[key, default: 0] += 1
        }
    } else if start[1] == end[1] {
        let x1 = Int(start[0])!
        let x2 = Int(end[0])!
        
        for x in min(x1, x2)...max(x1, x2) {
            let key = "\(x),\(start[1])"
            points[key, default: 0] += 1
        }
    }
}

let overlapCount = points.values.filter { $0 >= 2 }.count
print(overlapCount)
