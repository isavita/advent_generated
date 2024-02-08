
def file = new File("input.txt")
def input = file.text.trim()

def result = solve(input)
println(result)

int solve(String input) {
    def (nums, boards) = parseInput(input)

    def lastWinningScore = -1
    def alreadyWon = [:]
    nums.each { n ->
        boards.eachWithIndex { b, bi ->
            if (alreadyWon.containsKey(bi)) {
                return
            }
            def didWin = b.pickNum(n)
            if (didWin) {
                lastWinningScore = b.score() * n
                alreadyWon[bi] = true
            }
        }
    }

    return lastWinningScore
}

class BoardState {
    int[][] board
    boolean[][] picked

    BoardState(int[][] board) {
        picked = new boolean[board.size()][board[0].size()]
        this.board = board
    }

    boolean pickNum(int num) {
        board.eachWithIndex { r, ri ->
            r.eachWithIndex { c, ci ->
                if (c == num) {
                    picked[ri][ci] = true
                }
            }
        }

        for (int i = 0; i < board.size(); i++) {
            boolean isFullRow = true
            boolean isFullCol = true

            for (int j = 0; j < board.size(); j++) {
                if (!picked[i][j]) {
                    isFullRow = false
                }

                if (!picked[j][i]) {
                    isFullCol = false
                }
            }

            if (isFullRow || isFullCol) {
                return true
            }
        }

        return false
    }

    int score() {
        int score = 0

        board.eachWithIndex { r, ri ->
            r.eachWithIndex { c, ci ->
                if (!picked[ri][ci]) {
                    score += c
                }
            }
        }

        return score
    }
}

def parseInput(String input) {
    def lines = input.split("\n\n")
    def nums = lines[0].split(",").collect { toInt(it) }
    def boards = []

    lines[1..-1].each { grid ->
        def b = []
        grid.split("\n").each { line ->
            line = line.replaceAll("  ", " ")
            while (line.startsWith(" ")) {
                line = line[1..-1]
            }
            def parts = line.split(" ")

            def row = parts.collect { toInt(it) }
            b << row
        }

        boards << new BoardState(b as int[][])
    }

    return [nums, boards]
}

int toInt(String s) {
    return s.toInteger()
}
