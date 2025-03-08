
import Foundation

func findPosition(mat: [[Character]], ch: Character) -> (Int, Int) {
    for i in 0..<mat.count {
        for j in 0..<mat[i].count {
            if mat[i][j] == ch {
                return (i, j)
            }
        }
    }
    return (-1, -1)
}

func ok(mat: [[Character]], st: (Int, Int), seq: String) -> Bool {
    var currI = st.0
    var currJ = st.1
    for ch in seq {
        if !(0 <= currI && currI < mat.count && 0 <= currJ && currJ < mat[currI].count) || mat[currI][currJ] == " " {
            return false
        }
        switch ch {
        case "^": currI -= 1
        case "v": currI += 1
        case "<": currJ -= 1
        case ">": currJ += 1
        default: break
        }
    }
    return true
}

func generateMoves(position: (Int, Int), objective: Character, pad: [[Character]]) -> String {
    let (objPosI, objPosJ) = findPosition(mat: pad, ch: objective)
    let (posI, posJ) = position
    
    var result = ""
    if posJ > objPosJ {
        result += String(repeating: "<", count: posJ - objPosJ)
    }
    if posI > objPosI {
        result += String(repeating: "^", count: posI - objPosI)
    }
    if posI < objPosI {
        result += String(repeating: "v", count: objPosI - posI)
    }
    if posJ < objPosJ {
        result += String(repeating: ">", count: objPosJ - posJ)
    }

    if !ok(mat: pad, st: position, seq: result) {
        result = ""
        if posJ < objPosJ {
            result += String(repeating: ">", count: objPosJ - posJ)
        }
        if posI > objPosI {
            result += String(repeating: "^", count: posI - objPosI)
        }
        if posI < objPosI {
            result += String(repeating: "v", count: objPosI - posI)
        }
        if posJ > objPosJ {
            result += String(repeating: "<", count: posJ - objPosJ)
        }
    }
    
    return result
}

func solve(code: String, robots: Int, keyPad: [[Character]], robotPad: [[Character]], maxRobots: Int, memo: inout [String: Int]) -> Int {
    let key = "\(code)_\(robots)_\(maxRobots)"
    if let val = memo[key] {
        return val
    }

    if robots <= 0 {
        return code.count
    }

    var ret = 0
    var posI = 3
    var posJ = 2
    if robots != maxRobots {
        posI = 0
    }

    for ch in code {
        var moves = ""
        if robots == maxRobots {
            moves = generateMoves(position: (posI, posJ), objective: ch, pad: keyPad)
            (posI, posJ) = findPosition(mat: keyPad, ch: ch)
        } else {
            moves = generateMoves(position: (posI, posJ), objective: ch, pad: robotPad)
            (posI, posJ) = findPosition(mat: robotPad, ch: ch)
        }
        ret += solve(code: moves + "A", robots: robots - 1, keyPad: keyPad, robotPad: robotPad, maxRobots: maxRobots, memo: &memo)
    }

    memo[key] = ret
    return ret
}

func main() {
    guard let content = try? String(contentsOfFile: "input.txt", encoding: .utf8) else {
        return
    }

    let maxRobots = 26
    let keyPad: [[Character]] = [
        ["7", "8", "9"],
        ["4", "5", "6"],
        ["1", "2", "3"],
        [" ", "0", "A"]
    ]
    let robotPad: [[Character]] = [
        [" ", "^", "A"],
        ["<", "v", ">"]
    ]

    var ret = 0
    let codes = content.components(separatedBy: "\n")

    for code in codes {
        let trimmedCode = code.trimmingCharacters(in: .whitespaces)
        if trimmedCode.isEmpty {
            continue
        }

        var numericPart = 0
        for char in trimmedCode {
            if let digit = Int(String(char)) {
                numericPart = numericPart * 10 + digit
            }
        }
        var memo: [String: Int] = [:]

        ret += solve(code: trimmedCode, robots: maxRobots, keyPad: keyPad, robotPad: robotPad, maxRobots: maxRobots, memo: &memo) * numericPart
    }

    print(ret)
}

main()
