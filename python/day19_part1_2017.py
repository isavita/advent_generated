with open("input.txt") as file:
    grid = [[c for c in line] for line in file]
    x, y = 0, 0
    for i in range(len(grid[0])):
        if grid[0][i] == '|':
            x = i
            break
    dx, dy = 0, 1
    letters = []
    while True:
        if x < 0 or x >= len(grid[0]) or y < 0 or y >= len(grid):
            break
        cell = grid[y][x]
        if cell == ' ':
            break
        if cell.isalpha():
            letters.append(cell)
        elif cell == '+':
            if dx == 0:
                if x > 0 and (grid[y][x-1].lower() == '-' or grid[y][x-1].isupper()):
                    dx, dy = -1, 0
                else:
                    dx, dy = 1, 0
            else:
                if y > 0 and (grid[y-1][x] == '|' or grid[y-1][x].isupper()):
                    dx, dy = 0, -1
                else:
                    dx, dy = 0, 1
        x += dx
        y += dy
    print(''.join(letters))