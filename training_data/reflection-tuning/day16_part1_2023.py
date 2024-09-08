def read_input(file_name):
    with open(file_name, 'r') as file:
        return [list(line.strip()) for line in file]

def energize_tiles(grid):
    rows, cols = len(grid), len(grid[0])
    energized = set()
    visited = set()

    def move_beam(r, c, dr, dc):
        while 0 <= r < rows and 0 <= c < cols:
            if (r, c, dr, dc) in visited:
                return
            visited.add((r, c, dr, dc))
            energized.add((r, c))
            
            char = grid[r][c]
            if char == '/':
                dr, dc = -dc, -dr
            elif char == '\\':
                dr, dc = dc, dr
            elif char == '|' and dc != 0:
                move_beam(r-1, c, -1, 0)
                move_beam(r+1, c, 1, 0)
                return
            elif char == '-' and dr != 0:
                move_beam(r, c-1, 0, -1)
                move_beam(r, c+1, 0, 1)
                return
            
            r, c = r + dr, c + dc

    move_beam(0, 0, 0, 1)  # Start from top-left, moving right
    return len(energized)

grid = read_input("input.txt")
result = energize_tiles(grid)
print(result)
