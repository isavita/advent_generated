
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let content = try String(contentsOf: fileURL)
let lines = content.components(separatedBy: .newlines)

var numbers: [[Int]] = []
for line in lines {
    let sides = line.components(separatedBy: " ")
    var row: [Int] = []
    for side in sides {
        if let num = Int(side) {
            row.append(num)
        }
    }
    numbers.append(row)
}

var validTriangles = 0
for i in 0..<numbers[0].count {
    for j in stride(from: 0, to: numbers.count, by: 3) {
        if j + 2 < numbers.count && isValidTriangle(numbers[j][i], numbers[j + 1][i], numbers[j + 2][i]) {
            validTriangles += 1
        }
    }
}

print(validTriangles)

func isValidTriangle(_ a: Int, _ b: Int, _ c: Int) -> Bool {
    return a + b > c && a + c > b && b + c > a
}
