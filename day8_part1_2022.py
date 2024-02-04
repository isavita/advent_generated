
from collections import defaultdict

def read_input():
    with open('input.txt', 'r') as file:
        return [list(map(int, line.strip())) for line in file]

def count_visible(grid):
    neighbors4 = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    visible = set()
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            p = (x, y)
            for n in neighbors4:
                next_x, next_y = p
                while True:
                    next_x += n[0]
                    next_y += n[1]
                    if 0 <= next_x < len(grid[y]) and 0 <= next_y < len(grid):
                        if grid[next_y][next_x] >= grid[y][x]:
                            break
                    else:
                        visible.add(p)
                        break
    return len(visible)

if __name__ == "__main__":
    grid = read_input()
    print(count_visible(grid))
