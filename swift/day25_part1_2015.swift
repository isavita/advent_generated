import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let data = try String(contentsOf: fileURL)

let pattern = "row (\\d+), column (\\d+)"
let regex = try NSRegularExpression(pattern: pattern)
let matches = regex.matches(in: data, range: NSRange(data.startIndex..., in: data))

guard matches.count == 1 else {
    fatalError("Invalid input format.")
}

let rowRange = Range(matches[0].range(at: 1), in: data)!
let columnRange = Range(matches[0].range(at: 2), in: data)!

let row = Int(data[rowRange])!
let column = Int(data[columnRange])!

func getPosition(row: Int, column: Int) -> Int {
    return (row + column - 2) * (row + column - 1) / 2 + column
}

func getCode(position: Int) -> Int {
    let startCode = 20151125
    let multiplier = 252533
    let modulus = 33554393

    var code = startCode
    for _ in 1..<position {
        code = (code * multiplier) % modulus
    }
    return code
}

let pos = getPosition(row: row, column: column)
let code = getCode(position: pos)

print(code)