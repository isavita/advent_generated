
import math
import sys

def solve(input):
    matrix = parse_input(input)
    origin_col = 0
    for i, c in enumerate(matrix[0]):
        if c == "+":
            origin_col = i
        matrix[len(matrix)-1][i] = "#"

    ans = 0
    while not drop_sand(matrix, origin_col):
        ans += 1
        if matrix[0][origin_col] == "o":
            break

    return ans

def parse_input(input):
    coord_sets = []
    lowest_col = math.inf
    highest_row = 0
    for line in input.split("\n"):
        raw_coords = line.split(" -> ")
        coords = []
        for raw_coord in raw_coords:
            raw_nums = raw_coord.split(",")
            col, row = int(raw_nums[0]), int(raw_nums[1])
            coord = [col, row]
            coords.append(coord)

            lowest_col = min(lowest_col, col)
            highest_row = max(highest_row, row)
        coord_sets.append(coords)

    ExtraLeftSpace = 200
    highest_col = 0
    for s, coord_set in enumerate(coord_sets):
        for i in range(len(coord_set)):
            coord_sets[s][i][0] -= lowest_col - ExtraLeftSpace
            highest_col = max(highest_col, coord_sets[s][i][0])

    matrix = [["." for _ in range(highest_col + ExtraLeftSpace * 2)] for _ in range(highest_row + 3)]

    for set in coord_sets:
        for i in range(1, len(set)):
            cols = [set[i-1][0], set[i][0]]
            rows = [set[i-1][1], set[i][1]]

            cols.sort()
            rows.sort()

            if cols[0] == cols[1]:
                for r in range(rows[0], rows[1]+1):
                    matrix[r][cols[0]] = "#"
            elif rows[0] == rows[1]:
                for c in range(cols[0], cols[1]+1):
                    matrix[rows[0]][c] = "#"

    origin_col = 500 - lowest_col + ExtraLeftSpace
    matrix[0][origin_col] = "+"

    return matrix

def drop_sand(matrix, origin_col):
    r, c = 0, origin_col

    while r < len(matrix)-1:
        below = matrix[r+1][c]
        diagonally_left = matrix[r+1][c-1]
        diagonally_right = matrix[r+1][c+1]
        if below == ".":
            r += 1
        elif diagonally_left == ".":
            r += 1
            c -= 1
        elif diagonally_right == ".":
            r += 1
            c += 1
        else:
            matrix[r][c] = "o"
            return False

    return True

with open("input.txt", "r") as file:
    input_data = file.read().strip()
    print(solve(input_data))
