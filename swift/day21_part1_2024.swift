
import Foundation

func findPosition(mat: [String], ch: Character) -> (Int, Int) {
    for (i, row) in mat.enumerated() {
        for (j, char) in row.enumerated() {
            if char == ch {
                return (i, j)
            }
        }
    }
    return (-1, -1)
}

func ok(mat: [String], st: (Int, Int), seq: String) -> Bool {
    var currI = st.0
    var currJ = st.1
    for ch in seq {
        if mat[currI][mat[currI].index(mat[currI].startIndex, offsetBy: currJ)] == " " {
            return false
        }
        switch ch {
        case "^":
            currI -= 1
        case "v":
            currI += 1
        case "<":
            currJ -= 1
        case ">":
            currJ += 1
        default:
            break
        }
        if currI < 0 || currI >= mat.count || currJ < 0 || currJ >= mat[0].count {
            return false
        }
    }
    return true
}

func generateMoves(position: (Int, Int), objective: Character, pad: [String]) -> String {
    let (objI, objJ) = findPosition(mat: pad, ch: objective)
    let (posI, posJ) = position
    var ret = ""

    if posJ > objJ {
        ret += String(repeating: "<", count: posJ - objJ)
    }
    if posI > objI {
        ret += String(repeating: "^", count: posI - objI)
    }
    if posI < objI {
        ret += String(repeating: "v", count: objI - posI)
    }
    if posJ < objJ {
        ret += String(repeating: ">", count: objJ - posJ)
    }

    if !ok(mat: pad, st: position, seq: ret) {
        ret = ""
        if posJ < objJ {
            ret += String(repeating: ">", count: objJ - posJ)
        }
        if posI > objI {
            ret += String(repeating: "^", count: posI - objI)
        }
        if posI < objI {
            ret += String(repeating: "v", count: objI - posI)
        }
        if posJ > objJ {
            ret += String(repeating: "<", count: posJ - objJ)
        }
    }
    return ret
}
func solve(code: String, robots: Int, keyPad: [String], robotPad: [String], maxRobots: Int, memo: inout [String: Int]) -> Int {
    if robots <= 0 {
        return code.count
    }
    
    let state = "\(code)_\(robots)"
    if let cached = memo[state] {
        return cached
    }

    var ret = 0
    var posI = 3
    var posJ = 2
    if robots != maxRobots {
        posI = 0
    }

    for ch in code {
        var moves: String
        if robots == maxRobots {
            moves = generateMoves(position: (posI, posJ), objective: ch, pad: keyPad)
            (posI, posJ) = findPosition(mat: keyPad, ch: ch)
        } else {
            moves = generateMoves(position: (posI, posJ), objective: ch, pad: robotPad)
            (posI, posJ) = findPosition(mat: robotPad, ch: ch)
        }
        ret += solve(code: moves + "A", robots: robots - 1, keyPad: keyPad, robotPad: robotPad, maxRobots: maxRobots, memo: &memo)
    }
    
    memo[state] = ret
    return ret
}
func main() {
    guard let content = try? String(contentsOfFile: "input.txt", encoding: .utf8) else {
        return
    }

    let maxRobots = 3
    let keyPad = ["789", "456", "123", " 0A"]
    let robotPad = [" ^A", "<v>"]

    var ret = 0
    let codes = content.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n")

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
        let sv = solve(code: trimmedCode, robots: maxRobots, keyPad: keyPad, robotPad: robotPad, maxRobots: maxRobots, memo: &memo)
        ret += sv * numericPart
    }

    print(ret)
}
main()
