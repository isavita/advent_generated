
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).components(separatedBy: .newlines)

var grid = input.map { Array($0) }
var x = grid[0].firstIndex(of: "|")!
var y = 0
var dx = 0
var dy = 1
var result = ""
var steps = 0

while grid[y][x] != " " {
    steps += 1
    let char = grid[y][x]
    if char == "+" {
        if dx == 0 {
            dx = (grid[y][x-1] == " ") ? 1 : -1
            dy = 0
        } else {
            dx = 0
            dy = (grid[y-1][x] == " ") ? 1 : -1
        }
    } else if char != "|" && char != "-" {
        result.append(char)
    }
    x += dx
    y += dy
}

print(result)
