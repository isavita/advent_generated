
import Foundation

struct BoardState {
    let board: [[Int]]
    var picked: [[Bool]]
    var rows: [Int]
    var cols: [Int]
    
    init(board: [[Int]]) {
        self.board = board
        self.picked = Array(repeating: Array(repeating: false, count: board[0].count), count: board.count)
        self.rows = Array(repeating: 0, count: board.count)
        self.cols = Array(repeating: 0, count: board[0].count)
    }
    
    mutating func pickNum(_ num: Int) -> Bool {
        for r in 0..<board.count {
            for c in 0..<board[0].count {
                if board[r][c] == num {
                    picked[r][c] = true
                    rows[r] += 1
                    cols[c] += 1
                    if rows[r] == board.count || cols[c] == board[0].count {
                        return true
                    }
                }
            }
        }
        return false
    }
    
    func score() -> Int {
        var score = 0
        for r in 0..<board.count {
            for c in 0..<board[0].count {
                if !picked[r][c] {
                    score += board[r][c]
                }
            }
        }
        return score
    }
}

func parseInput(_ input: String) -> (nums: [Int], boards: [BoardState]) {
    let lines = input.components(separatedBy: "\n\n")
    let nums = lines[0].components(separatedBy: ",").map { Int($0)! }
    var boards: [BoardState] = []
    for grid in lines.dropFirst() {
        let b = grid.components(separatedBy: "\n").map {
            $0.replacingOccurrences(of: "  ", with: " ").trimmingCharacters(in: .whitespaces).components(separatedBy: " ").map { Int($0)! }
        }
        boards.append(BoardState(board: b))
    }
    return (nums, boards)
}

func solve(_ input: String) -> Int {
    var (nums, boards) = parseInput(input)
    var lastWinningScore = -1
    var alreadyWon = Set<Int>()
    for n in nums {
        for (bi, var b) in boards.enumerated() {
            if alreadyWon.contains(bi) {
                continue
            }
            let didWin = b.pickNum(n)
            if didWin {
                lastWinningScore = b.score() * n
                alreadyWon.insert(bi)
            }
            boards[bi] = b
        }
    }
    return lastWinningScore
}

let file = "input.txt"
let input = try String(contentsOfFile: file).trimmingCharacters(in: .whitespacesAndNewlines)
let result = solve(input)
print(result)
