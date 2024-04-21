import Foundation

let totalRows = 400000

func readFirstRow(from filename: String) -> String? {
    do {
        let content = try String(contentsOfFile: filename, encoding: .utf8)
        return content.components(separatedBy: "\n").first
    } catch {
        return nil
    }
}

func countSafeTiles(in firstRow: String, totalRows: Int) -> Int {
    var currentRow = Array(firstRow)
    var safeCount = countChar(in: currentRow, char: ".")
    
    for _ in 1..<totalRows {
        var nextRow: [Character] = []
        for j in 0..<currentRow.count {
            let left = safeIfOutOfBounds(j-1, in: currentRow)
            let center = currentRow[j]
            let right = safeIfOutOfBounds(j+1, in: currentRow)
            
            if isTrap(left: left, center: center, right: right) {
                nextRow.append("^")
            } else {
                nextRow.append(".")
                safeCount += 1
            }
        }
        currentRow = nextRow
    }
    return safeCount
}

func isTrap(left: Character, center: Character, right: Character) -> Bool {
    return (left == "^" && center == "^" && right == ".") ||
           (center == "^" && right == "^" && left == ".") ||
           (left == "^" && center == "." && right == ".") ||
           (right == "^" && center == "." && left == ".")
}

func safeIfOutOfBounds(_ index: Int, in row: [Character]) -> Character {
    if index < 0 || index >= row.count {
        return "."
    }
    return row[index]
}

func countChar(in str: [Character], char: Character) -> Int {
    return str.filter { $0 == char }.count
}

if let firstRow = readFirstRow(from: "input.txt") {
    let safeTilesCount = countSafeTiles(in: firstRow, totalRows: totalRows)
    print(safeTilesCount)
} else {
    print("Failed to read the first row")
}