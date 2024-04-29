def inputFile = new File('input.txt')
def lines = inputFile.readLines()

def numbers = lines[0].split(',').collect { it.toInteger() }
def boards = []

def currentBoard = []
for (int i = 2; i < lines.size(); i++) {
    def line = lines[i].trim()
    if (line) {
        currentBoard.add(line.split('\\s+').collect { it.toInteger() })
    } else {
        boards.add(currentBoard)
        currentBoard = []
    }
}
boards.add(currentBoard)

def winningBoard
def winningNumber
def winningScore

outer: for (number in numbers) {
    for (board in boards) {
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                if (board[i][j] == number) {
                    board[i][j] = -1
                }
            }
        }
        if (hasWon(board)) {
            winningBoard = board
            winningNumber = number
            winningScore = calculateScore(winningBoard) * winningNumber
            break outer
        }
    }
}

println "Final score: $winningScore"

def hasWon(board) {
    for (int i = 0; i < 5; i++) {
        if (board[i][0..4].every { it == -1 } || board[0..4].every { board[it][i] == -1 }) {
            return true
        }
    }
    return false
}

def calculateScore(board) {
    def sum = 0
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            if (board[i][j] != -1) {
                sum += board[i][j]
            }
        }
    }
    return sum
}