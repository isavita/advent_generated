
import Foundation

struct Machine {
    let target: [Int]
    let buttons: [[Int]]
}

func parseInput(_ path: String) throws -> [Machine] {
    let text = try String(contentsOfFile: path, encoding: .utf8)
    var machines = [Machine]()
    let diagramRE = try NSRegularExpression(pattern: #"^\[([.#]+)\]"#)
    let buttonRE  = try NSRegularExpression(pattern: #"\(([\d,]+)\)"#)

    for raw in text.split(separator: "\n", omittingEmptySubsequences: true) {
        let line = raw.trimmingCharacters(in: .whitespaces)
        guard let dMatch = diagramRE.firstMatch(in: line, range: NSRange(line.startIndex..., in: line)),
              let dRange = Range(dMatch.range(at: 1), in: line) else { continue }

        let diag = line[dRange]
        let target = diag.map { $0 == "#" ? 1 : 0 }

        var buttons = [[Int]]()
        let btnMatches = buttonRE.matches(in: line, range: NSRange(line.startIndex..., in: line))
        for m in btnMatches {
            guard let r = Range(m.range(at: 1), in: line) else { continue }
            let nums = line[r].split(separator: ",").compactMap { Int($0.trimmingCharacters(in: .whitespaces)) }
            buttons.append(nums)
        }
        machines.append(Machine(target: target, buttons: buttons))
    }
    return machines
}

func solveMachine(_ m: Machine) -> Int {
    let rows = m.target.count
    let cols = m.buttons.count
    var mat = Array(repeating: Array(repeating: 0, count: cols + 1), count: rows)

    for r in 0..<rows {
        mat[r][cols] = m.target[r]
        for (c, btn) in m.buttons.enumerated() {
            if btn.contains(r) { mat[r][c] = 1 }
        }
    }
    return gaussianMinWeight(&mat, rows, cols)
}

func gaussianMinWeight(_ mat: inout [[Int]], _ R: Int, _ C: Int) -> Int {
    var pivotRow = 0
    var colIsPivot = Array(repeating: false, count: C)

    for c in 0..<C where pivotRow < R {
        var sel: Int? = nil
        for r in pivotRow..<R where mat[r][c] == 1 { sel = r; break }
        guard let s = sel else { continue }

        mat.swapAt(pivotRow, s)
        for r in 0..<R where r != pivotRow && mat[r][c] == 1 {
            for k in c...C { mat[r][k] ^= mat[pivotRow][k] }
        }
        colIsPivot[c] = true
        pivotRow += 1
    }

    for r in pivotRow..<R where mat[r][C] == 1 { return -1 }

    var freeVars = [Int]()
    for c in 0..<C where !colIsPivot[c] { freeVars.append(c) }

    let numFree = freeVars.count
    let limit = 1 << numFree
    var best = Int.max

    for mask in 0..<limit {
        var x = Array(repeating: 0, count: C)
        var weight = 0
        for (j, v) in freeVars.enumerated() where ((mask >> j) & 1) == 1 {
            x[v] = 1; weight += 1
        }

        var curPivotRow = 0
        for c in 0..<C where colIsPivot[c] {
            var val = mat[curPivotRow][C]
            for k in (c+1)..<C where mat[curPivotRow][k] == 1 {
                val ^= x[k]
            }
            x[c] = val
            if val == 1 { weight += 1 }
            curPivotRow += 1
        }
        if weight < best { best = weight }
    }
    return best
}

do {
    let machines = try parseInput("input.txt")
    var total = 0
    for m in machines {
        let p = solveMachine(m)
        if p >= 0 { total += p }
    }
    print("Fewest total button presses: \(total)")
} catch {
    fatalError("Failed to read input: \(error)")
}
