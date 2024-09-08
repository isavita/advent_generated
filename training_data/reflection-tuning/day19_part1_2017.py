def follow_path(grid):
    directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]  # down, right, up, left
    y, x = 0, grid[0].index('|')  # Find starting point
    dy, dx = directions[0]  # Start moving down
    letters = []
    steps = 0

    while True:
        y += dy
        x += dx
        steps += 1

        if y < 0 or y >= len(grid) or x < 0 or x >= len(grid[y]) or grid[y][x] == ' ':
            break  # End of path

        char = grid[y][x]
        if char.isalpha():
            letters.append(char)
        elif char == '+':
            for new_dy, new_dx in directions:
                if (new_dy, new_dx) != (-dy, -dx):  # Don't go back
                    ny, nx = y + new_dy, x + new_dx
                    if 0 <= ny < len(grid) and 0 <= nx < len(grid[ny]) and grid[ny][nx] != ' ':
                        dy, dx = new_dy, new_dx
                        break

    return ''.join(letters), steps

# Read input
with open('input.txt', 'r') as file:
    grid = file.read().splitlines()

# Solve and print result
letters, steps = follow_path(grid)
print(f"Letters: {letters}")
print(f"Steps: {steps}")
