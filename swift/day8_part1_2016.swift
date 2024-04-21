import Foundation

let screenWidth = 50
let screenHeight = 6

var screen = [[Bool]](repeating: [Bool](repeating: false, count: screenWidth), count: screenHeight)

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    let instructions = fileContent.components(separatedBy: "\n")
    
    for instruction in instructions {
        processInstruction(instruction: instruction)
    }
    
    print(countLitPixels())
}

func processInstruction(instruction: String) {
    let rectRegex = try! NSRegularExpression(pattern: "rect (\\d+)x(\\d+)", options: [])
    let rotateRowRegex = try! NSRegularExpression(pattern: "rotate row y=(\\d+) by (\\d+)", options: [])
    let rotateColumnRegex = try! NSRegularExpression(pattern: "rotate column x=(\\d+) by (\\d+)", options: [])
    
    if rectRegex.firstMatch(in: instruction, options: [], range: NSRange(location: 0, length: instruction.count)) != nil {
        let matches = rectRegex.matches(in: instruction, options: [], range: NSRange(location: 0, length: instruction.count))
        guard let match = matches.first else { return }
        let a = Int(instruction[Range(match.range(at: 1), in: instruction)!])!
        let b = Int(instruction[Range(match.range(at: 2), in: instruction)!])!
        rect(a: a, b: b)
    } else if rotateRowRegex.firstMatch(in: instruction, options: [], range: NSRange(location: 0, length: instruction.count)) != nil {
        let matches = rotateRowRegex.matches(in: instruction, options: [], range: NSRange(location: 0, length: instruction.count))
        guard let match = matches.first else { return }
        let a = Int(instruction[Range(match.range(at: 1), in: instruction)!])!
        let b = Int(instruction[Range(match.range(at: 2), in: instruction)!])!
        rotateRow(row: a, shift: b)
    } else if rotateColumnRegex.firstMatch(in: instruction, options: [], range: NSRange(location: 0, length: instruction.count)) != nil {
        let matches = rotateColumnRegex.matches(in: instruction, options: [], range: NSRange(location: 0, length: instruction.count))
        guard let match = matches.first else { return }
        let a = Int(instruction[Range(match.range(at: 1), in: instruction)!])!
        let b = Int(instruction[Range(match.range(at: 2), in: instruction)!])!
        rotateColumn(col: a, shift: b)
    }
}

func rect(a: Int, b: Int) {
    for y in 0..<b {
        for x in 0..<a {
            screen[y][x] = true
        }
    }
}

func rotateRow(row: Int, shift: Int) {
    var temp = [Bool](repeating: false, count: screenWidth)
    for i in 0..<screenWidth {
        temp[(i+shift)%screenWidth] = screen[row][i]
    }
    screen[row] = temp
}

func rotateColumn(col: Int, shift: Int) {
    var temp = [Bool](repeating: false, count: screenHeight)
    for i in 0..<screenHeight {
        temp[(i+shift)%screenHeight] = screen[i][col]
    }
    for i in 0..<screenHeight {
        screen[i][col] = temp[i]
    }
}

func countLitPixels() -> Int {
    var count = 0
    for row in screen {
        for pixel in row {
            if pixel {
                count += 1
            }
        }
    }
    return count
}