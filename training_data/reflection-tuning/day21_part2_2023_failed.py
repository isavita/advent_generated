def parse_input(input_str):
    return [list(line) for line in input_str.strip().split('\n')]

def count_reachable_plots(grid, steps):
    height, width = len(grid), len(grid[0])
    start = next((i, j) for i, row in enumerate(grid) for j, cell in enumerate(row) if cell == 'S')
    
    even_count = odd_count = 0
    visited = set()
    queue = [(start[0], start[1], 0)]
    
    while queue:
        x, y, step = queue.pop(0)
        if (x, y, step % 2) in visited:
            continue
        visited.add((x, y, step % 2))
        
        if step % 2 == 0:
            even_count += 1
        else:
            odd_count += 1
        
        if step == steps:
            continue
        
        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < height and 0 <= ny < width and grid[nx][ny] != '#':
                queue.append((nx, ny, step + 1))
    
    return even_count if steps % 2 == 0 else odd_count

def solve(input_str, total_steps):
    grid = parse_input(input_str)
    size = len(grid)
    half_size = size // 2
    
    # Calculate the number of full grid repetitions
    n = (total_steps - half_size) // size
    
    # Count reachable plots for odd and even full grids
    odd_full = count_reachable_plots(grid, size * 2 + 1)
    even_full = count_reachable_plots(grid, size * 2)
    
    # Calculate the number of odd and even full grids
    odd_grids = (n + 1) ** 2
    even_grids = n ** 2
    
    # Calculate partial grids at the edges
    top = count_reachable_plots(grid, half_size - 1)
    right = count_reachable_plots(grid, half_size - 1)
    bottom = count_reachable_plots(grid, half_size - 1)
    left = count_reachable_plots(grid, half_size - 1)
    
    # Calculate corner cases
    small_corner = count_reachable_plots(grid, half_size // 2 - 1)
    large_corner = count_reachable_plots(grid, 3 * half_size // 2 - 1)
    
    # Combine all parts
    result = (odd_grids * odd_full +
              even_grids * even_full +
              (top + right + bottom + left) +
              n * (small_corner * 4 + large_corner * 4))
    
    return result

# Example usage:
input_str = """
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"""
print(solve(input_str, 26501365))
