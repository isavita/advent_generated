
import Foundation

var screen = Array(repeating: Array(repeating: ".", count: 50), count: 6)

func rect(_ a: Int, _ b: Int) {
    for i in 0..<b {
        for j in 0..<a {
            screen[i][j] = "#"
        }
    }
}

func rotateRow(_ y: Int, by b: Int) {
    let row = screen[y]
    let rotatedRow = Array(row.suffix(b) + row.prefix(50 - b))
    screen[y] = rotatedRow
}

func rotateColumn(_ x: Int, by b: Int) {
    var column = [String]()
    for i in 0..<6 {
        column.append(screen[i][x])
    }
    let rotatedColumn = Array(column.suffix(b) + column.prefix(6 - b))
    for i in 0..<6 {
        screen[i][x] = rotatedColumn[i]
    }
}

if let input = try? String(contentsOfFile: "input.txt") {
    let instructions = input.components(separatedBy: .newlines)
    
    for instruction in instructions {
        let parts = instruction.components(separatedBy: " ")
        if parts[0] == "rect" {
            let dimensions = parts[1].components(separatedBy: "x")
            let a = Int(dimensions[0])!
            let b = Int(dimensions[1])!
            rect(a, b)
        } else if parts[0] == "rotate" {
            let ab = parts[2].components(separatedBy: "=")
            let xy = Int(ab[1])!
            let by = Int(parts[4])!
            if parts[1] == "row" {
                rotateRow(xy, by: by)
            } else if parts[1] == "column" {
                rotateColumn(xy, by: by)
            }
        }
    }
    
    let litPixels = screen.flatMap({ $0 }).filter({ $0 == "#" }).count
    print(litPixels)
    
    for i in 0..<6 {
        print(screen[i].joined())
    }
}
