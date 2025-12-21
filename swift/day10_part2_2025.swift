
import Foundation

let INF = Int.max / 4
let eps = 1e-9

var buttons = [[Int]]()
var btnSize = [Int]()
var targets = [Int]()
var numCounters = 0, numButtons = 0

var matrix = [[Double]]()
var pivotCol = [Int]()
var isPivot = [Int]()
var pivotRows = [Int]()
var freeVars = [Int]()
var numFree = 0
var maxPresses = [Int]()
var freeValues = [Int]()
var bestResult = INF

func parseLine(_ line: String) {
    numCounters = 0; numButtons = 0
    buttons.removeAll(); btnSize.removeAll(); targets.removeAll()
    var i = line.startIndex
    while i < line.endIndex {
        let ch = line[i]
        if ch == "(" {
            var btn = [Int]()
            i = line.index(after: i)
            while i < line.endIndex && line[i] != ")" {
                var num = 0
                while i < line.endIndex, let d = line[i].wholeNumberValue {
                    num = num * 10 + d; i = line.index(after: i)
                }
                btn.append(num)
                if i < line.endIndex && line[i] == "," { i = line.index(after: i) }
            }
            if i < line.endIndex { i = line.index(after: i) }
            buttons.append(btn); btnSize.append(btn.count); numButtons += 1
        } else if ch == "{" {
            i = line.index(after: i)
            while i < line.endIndex && line[i] != "}" {
                var num = 0
                while i < line.endIndex, let d = line[i].wholeNumberValue {
                    num = num * 10 + d; i = line.index(after: i)
                }
                targets.append(num); numCounters += 1
                if i < line.endIndex && line[i] == "," { i = line.index(after: i) }
            }
            break
        } else {
            i = line.index(after: i)
        }
    }
}

func gauss() {
    matrix = Array(repeating: Array(repeating: 0.0, count: numButtons + 1), count: numCounters)
    for j in 0..<numCounters { matrix[j][numButtons] = Double(targets[j]) }
    for i in 0..<numButtons {
        for c in buttons[i] where c < numCounters { matrix[c][i] = 1.0 }
    }
    pivotCol = Array(repeating: -1, count: numCounters)
    var row = 0
    for col in 0..<numButtons where row < numCounters {
        var maxRow = row
        for r in (row+1)..<numCounters where abs(matrix[r][col]) > abs(matrix[maxRow][col]) { maxRow = r }
        if abs(matrix[maxRow][col]) < eps { continue }
        matrix.swapAt(row, maxRow)
        let scale = matrix[row][col]
        for c in col...numButtons { matrix[row][c] /= scale }
        for r in 0..<numCounters where r != row && abs(matrix[r][col]) > eps {
            let factor = matrix[r][col]
            for c in col...numButtons { matrix[r][c] -= factor * matrix[row][c] }
        }
        pivotCol[row] = col
        row += 1
    }
    let rank = row
    isPivot = Array(repeating: 0, count: numButtons)
    pivotRows = Array(repeating: -1, count: numButtons)
    for r in 0..<rank {
        let c = pivotCol[r]
        if c >= 0 { isPivot[c] = 1; pivotRows[c] = r }
    }
    freeVars.removeAll()
    for i in 0..<numButtons where isPivot[i] == 0 { freeVars.append(i) }
    numFree = freeVars.count
    maxPresses = Array(repeating: INF, count: numButtons)
    for i in 0..<numButtons {
        var mp = INF
        for c in buttons[i] where c < numCounters { mp = min(mp, targets[c]) }
        maxPresses[i] = (mp == INF) ? 0 : mp
    }
    freeVars.sort { maxPresses[$0] < maxPresses[$1] }
    freeValues = Array(repeating: 0, count: numFree)
}

func computePivots(_ presses: inout [Int]) -> Int {
    presses = Array(repeating: 0, count: numButtons)
    for i in 0..<numFree { presses[freeVars[i]] = freeValues[i] }
    for r in stride(from: numCounters-1, through: 0, by: -1) {
        let col = pivotCol[r]
        if col < 0 { continue }
        var val = matrix[r][numButtons]
        for c in (col+1)..<numButtons { val -= matrix[r][c] * Double(presses[c]) }
        let intVal = Int(round(val))
        if abs(val - Double(intVal)) > 1e-6 || intVal < 0 || intVal > maxPresses[col] { return 0 }
        presses[col] = intVal
    }
    return presses.reduce(0, +)
}

func enumerate(_ idx: Int, _ curSum: Int) {
    if curSum >= bestResult { return }
    if idx == numFree {
        var presses = [Int]()
        let sum = computePivots(&presses)
        if sum > 0 && sum < bestResult { bestResult = sum }
        return
    }
    let fv = freeVars[idx]
    let limit = maxPresses[fv]
    for v in 0...limit {
        freeValues[idx] = v
        enumerate(idx + 1, curSum + v)
    }
}

func solve() -> Int {
    gauss()
    bestResult = INF
    enumerate(0, 0)
    return bestResult == INF ? -1 : bestResult
}

let path = "input.txt"
guard let content = try? String(contentsOfFile: path) else { exit(0) }
let lines = content.components(separatedBy: .newlines)
var total = 0
for line in lines where !line.trimmingCharacters(in: .whitespaces).isEmpty {
    parseLine(line)
    let res = solve()
    if res > 0 { total += res }
}
print(total)
