
class BingoBoard:
    def __init__(self, numbers):
        self.numbers = numbers
        self.marked = [[False for _ in range(5)] for _ in range(5)]

    def mark(self, number):
        for i in range(5):
            for j in range(5):
                if self.numbers[i][j] == number:
                    self.marked[i][j] = True

    def has_won(self):
        for i in range(5):
            if all(self.marked[i]) or all(row[i] for row in self.marked):
                return True
        return False

    def unmarked_sum(self):
        return sum(self.numbers[i][j] for i in range(5) for j in range(5) if not self.marked[i][j])

file = open("input.txt", "r")
numbers = file.readline().strip().split(",")
boards = []

for line in file:
    board = []
    for _ in range(5):
        line = list(map(int, file.readline().split()))
        board.append(line)
    boards.append(BingoBoard(board))

winning_board = None
winning_number = 0

for number in numbers:
    n = int(number)
    for board in boards:
        board.mark(n)
        if board.has_won():
            winning_board = board
            winning_number = n
            break
    if winning_board:
        break

print(winning_board.unmarked_sum() * winning_number)
