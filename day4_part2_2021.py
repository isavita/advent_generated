
def main():
    with open("input.txt", "r") as file:
        input_data = file.read().strip()

    result = solve(input_data)
    print(result)

def solve(input_data):
    nums, boards = parse_input(input_data)

    last_winning_score = -1
    already_won = {}
    for n in nums:
        for bi, b in enumerate(boards):
            if bi in already_won:
                continue
            did_win = b.pick_num(n)
            if did_win:
                last_winning_score = b.score() * n
                already_won[bi] = True

    return last_winning_score

class BoardState:
    def __init__(self, board):
        self.board = board
        self.picked = [[False for _ in row] for row in board]

    def pick_num(self, num):
        for r, rows in enumerate(self.board):
            for c, v in enumerate(rows):
                if v == num:
                    self.picked[r][c] = True

        for i in range(len(self.board)):
            is_full_row, is_full_col = True, True
            for j in range(len(self.board)):
                if not self.picked[i][j]:
                    is_full_row = False
                if not self.picked[j][i]:
                    is_full_col = False
            if is_full_row or is_full_col:
                return True
        return False

    def score(self):
        score = 0
        for r, rows in enumerate(self.board):
            for c, v in enumerate(rows):
                if not self.picked[r][c]:
                    score += v
        return score

def parse_input(input_data):
    nums = [int(v) for v in input_data.split("\n\n")[0].split(",")]
    boards = []
    for grid in input_data.split("\n\n")[1:]:
        b = []
        for line in grid.split("\n"):
            line = line.replace("  ", " ")
            while line[0] == ' ':
                line = line[1:]
            parts = line.split(" ")
            row = [int(p) for p in parts]
            b.append(row)
        boards.append(BoardState(b))
    return nums, boards

if __name__ == "__main__":
    main()
