import Foundation

func readFileToMatrix(filePath: String) -> [[Character]] {
    do {
        let fileContent = try String(contentsOfFile: filePath, encoding: .utf8)
        let matrix = fileContent.components(separatedBy: "\n").map { $0.compactMap { $0 } }
        return matrix
    } catch {
        print("Error reading file:", error)
        return []
    }
}

func sumOfPartNumbers(matrix: [[Character]]) -> Int {
    var sum = 0
    var visited = [[Bool]](repeating: [Bool](repeating: false, count: matrix[0].count), count: matrix.count)
    
    for y in 0..<matrix.count {
        for x in 0..<matrix[y].count {
            if !visited[y][x] && CharacterSet.decimalDigits.contains(matrix[y][x].unicodeScalars.first!) {
                let (number, length) = extractNumber(matrix: matrix, x: x, y: y)
                if isAdjacentToSymbol(matrix: matrix, x: x, y: y, length: length) {
                    sum += number
                }
                for i in 0..<length {
                    visited[y][x+i] = true
                }
            }
        }
    }
    return sum
}

func extractNumber(matrix: [[Character]], x: Int, y: Int) -> (Int, Int) {
    var numberStr = ""
    var x = x
    while x < matrix[y].count && CharacterSet.decimalDigits.contains(matrix[y][x].unicodeScalars.first!) {
        numberStr += String(matrix[y][x])
        x += 1
    }
    guard let number = Int(numberStr) else { return (0, 0) }
    return (number, numberStr.count)
}

func isAdjacentToSymbol(matrix: [[Character]], x: Int, y: Int, length: Int) -> Bool {
    for i in 0..<length {
        if checkAdjacent(matrix: matrix, x: x+i, y: y) {
            return true
        }
    }
    return false
}

func checkAdjacent(matrix: [[Character]], x: Int, y: Int) -> Bool {
    for dy in -1...1 {
        for dx in -1...1 {
            let adjX = x + dx
            let adjY = y + dy
            if adjY >= 0 && adjY < matrix.count && adjX >= 0 && adjX < matrix[adjY].count {
                if !CharacterSet.decimalDigits.contains(matrix[adjY][adjX].unicodeScalars.first!) && matrix[adjY][adjX] != "." {
                    return true
                }
            }
        }
    }
    return false
}

let matrix = readFileToMatrix(filePath: "input.txt")
let sum = sumOfPartNumbers(matrix: matrix)
print(sum)