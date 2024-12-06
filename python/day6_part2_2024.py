class State:
    def __init__(self, x, y, dir_idx):
        self.x = x
        self.y = y
        self.dir_idx = dir_idx
    
    def __eq__(self, other):
        return (self.x == other.x and 
                self.y == other.y and 
                self.dir_idx == other.dir_idx)
    
    def __hash__(self):
        return hash((self.x, self.y, self.dir_idx))

def loops(grid, start_x, start_y, start_dir):
    h, w = len(grid), len(grid[0])
    dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]  # up, right, down, left
    
    x, y = start_x, start_y
    dir_idx = start_dir
    seen = set()
    
    # Use fixed number of steps as loop detection
    for _ in range(2000000):
        state = State(x, y, dir_idx)
        if state in seen:
            return True
        seen.add(state)
        
        dir_x, dir_y = dirs[dir_idx]
        nx, ny = x + dir_x, y + dir_y
        
        # Check if leaving grid
        if nx < 0 or nx >= w or ny < 0 or ny >= h:
            return False
            
        # If obstacle found, turn right
        if grid[ny][nx] == '#':
            dir_idx = (dir_idx + 1) % 4
            continue
            
        # Move to new position
        x, y = nx, ny
    
    return False

def find_loop_positions(input_str):
    # Create grid
    grid = [list(line.strip()) for line in input_str.strip().split('\n') if line.strip()]
    h, w = len(grid), len(grid[0])
    
    # Find starting position and direction
    start_x = start_y = None
    start_dir = 0  # Default to up
    for i in range(h):
        for j in range(w):
            if grid[i][j] == '^':
                start_x, start_y = j, i
                start_dir = 0
            elif grid[i][j] == '>':
                start_x, start_y = j, i
                start_dir = 1
            elif grid[i][j] == 'v':
                start_x, start_y = j, i
                start_dir = 2
            elif grid[i][j] == '<':
                start_x, start_y = j, i
                start_dir = 3
    
    # Convert starting position to empty space
    grid[start_y][start_x] = '.'
    
    # Count positions that create loops
    can_loop = 0
    for y in range(h):
        for x in range(w):
            if x == start_x and y == start_y:
                continue
            if grid[y][x] != '.':
                continue
                
            # Try placing obstacle
            grid[y][x] = '#'
            if loops(grid, start_x, start_y, start_dir):
                can_loop += 1
            grid[y][x] = '.'
    
    return can_loop

# read the input file
with open('input.txt', 'r') as file:
    input_str = file.read()

print(find_loop_positions(input_str))
