
import Foundation

struct Row {
    var springs: String
    var group: [Int]
}

func parseInput(input: [String]) -> [Row] {
    var rows: [Row] = []
    for line in input {
        let parts = line.components(separatedBy: " ")
        let springs = parts[0]
        let ints = parseStringToInts(numbersLine: parts[1])
        
        let row = Row(springs: springs, group: ints)
        rows.append(row)
    }
    return rows
}

func parseStringToInts(numbersLine: String) -> [Int] {
    var numbers: [Int] = []
    let numbersParts = numbersLine.components(separatedBy: ",")
    for numberStr in numbersParts {
        if let number = Int(numberStr) {
            numbers.append(number)
        } else {
            fatalError("Error converting string to int")
        }
    }
    return numbers
}

func countArrangementsRecursive(row: Row, iSprings: Int, iGroup: Int, iContiguousDamaged: Int, cache: inout [Int: Int]) -> Int {
    if iSprings == row.springs.count {
        if iGroup == row.group.count && iContiguousDamaged == 0 {
            return 1
        } else if iGroup == row.group.count - 1 && iContiguousDamaged == row.group[iGroup] {
            return 1
        }
        return 0
    }
    
    let cacheKey = iSprings * 1000 + iGroup * 100 + iContiguousDamaged
    if let val = cache[cacheKey] {
        return val
    }
    
    var res = 0
    let char = row.springs[row.springs.index(row.springs.startIndex, offsetBy: iSprings)]
    if char == "." || char == "?" {
        if iContiguousDamaged == 0 {
            res += countArrangementsRecursive(row: row, iSprings: iSprings + 1, iGroup: iGroup, iContiguousDamaged: iContiguousDamaged, cache: &cache)
        } else if iContiguousDamaged == row.group[iGroup] {
            res += countArrangementsRecursive(row: row, iSprings: iSprings + 1, iGroup: iGroup + 1, iContiguousDamaged: 0, cache: &cache)
        }
    }
    if char == "#" || char == "?" {
        if iGroup < row.group.count && iContiguousDamaged < row.group[iGroup] {
            res += countArrangementsRecursive(row: row, iSprings: iSprings + 1, iGroup: iGroup, iContiguousDamaged: iContiguousDamaged + 1, cache: &cache)
        }
    }
    
    cache[cacheKey] = res
    return res
}

func countArrangements(row: Row) -> Int {
    var cache: [Int: Int] = [:]
    return countArrangementsRecursive(row: row, iSprings: 0, iGroup: 0, iContiguousDamaged: 0, cache: &cache)
}

func unfoldRow(row: Row, unfoldingFactor: Int) -> Row {
    var newRow = Row(springs: row.springs, group: row.group)
    
    for _ in 1..<unfoldingFactor {
        newRow.springs += "?" + row.springs
        newRow.group += row.group
    }
    
    return newRow
}

func solve(input: [String]) -> Int {
    let rows = parseInput(input: input)
    
    var res = 0
    for row in rows {
        res += countArrangements(row: row)
    }
    
    return res
}

func readFile(fileName: String) -> [String] {
    do {
        let fileContent = try String(contentsOfFile: fileName)
        return fileContent.components(separatedBy: "\n")
    } catch {
        fatalError("Error reading file")
    }
}

let input = readFile(fileName: "input.txt")
print(solve(input: input))
