
def solve():
    with open("input.txt", "r") as f:
        lines = f.read().splitlines()
    
    grid = []
    moves = ""
    reading_map = True
    
    for line in lines:
        if reading_map:
            if "#" in line:
                grid.append(list(line))
            else:
                reading_map = False
                moves += line
        else:
            moves += line
    
    rows = len(grid)
    cols = len(grid[0])
    
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '@':
                robot_r, robot_c = r, c
                break
    
    dirs = {
        '^': (-1, 0),
        'v': (1, 0),
        '<': (0, -1),
        '>': (0, 1)
    }
    
    def push_boxes(r, c, dr, dc):
        nr, nc = r + dr, c + dc
        if grid[nr][nc] == '#':
            return False
        if grid[nr][nc] == 'O':
            if not push_boxes(nr, nc, dr, dc):
                return False
        if grid[nr][nc] == '.':
            grid[nr][nc] = 'O'
            grid[r][c] = '.'
            return True
        return False
    
    for move in moves:
        dr, dc = dirs[move]
        nr, nc = robot_r + dr, robot_c + dc
        if grid[nr][nc] == '#':
            continue
        elif grid[nr][nc] == 'O':
            if not push_boxes(nr, nc, dr, dc):
                continue
        if grid[nr][nc] == '.' or grid[nr][nc] == 'O':
            grid[robot_r][robot_c], grid[nr][nc] = '.', '@'
            robot_r, robot_c = nr, nc
    
    total_sum = 0
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 'O':
                total_sum += r * 100 + c
    
    print(total_sum)

solve()
