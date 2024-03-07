# Python Task:
# Read Input From A File
import sys, io
from pathlib import Path

# Function To Read The Input File
def read_input(file="input.txt"):
    with open(Path(__file__).parent / file) as f:
        return [line.rstrip() for line in f]

def solve():
    # Initializing Global Variables And Structures
    grid = []
    dx, dy = 0, 1
    x, y, steps = 0, 0, 0

    with open("input.txt") as file:
        for line in file:
            grid.append([c for c in list(line)])

    # Initialize The Initial Position Of X And Y Coordinates
    for i in range(len(grid[0])):
        if grid[0][i] == '|':
            x = i
            break

    while True:
        cell = grid[y][x]

        # Move In Different Direction Based On The Cell's Value
        if cell == ' ':
            # We Reached The End, So Break
            break
        elif cell == '+' and dx == 0:
            if x > 0 and (grid[y][x-1] == '-' or ord('A') <= ord(cell) and ord(cell) <= ord('Z')):
                dx, dy = -1, 0
            else:
                dx, dy = 1, 0
        elif cell == '+' and dx != 0:
            if y > 0 and (grid[y-1][x] == '|' or ord('A') <= ord(cell) and ord(cell) <= ord('Z')):
                dx, dy = 0, -1
            else:
                dx, dy = 0, 1
        x += dx
        y += dy
        steps += 1

    print(steps)

if __name__ == "__main__":
    solve()