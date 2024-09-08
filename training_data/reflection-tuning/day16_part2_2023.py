from collections import deque

def parse_input(filename):
    with open(filename, 'r') as file:
        return [list(line.strip()) for line in file]

def simulate_beam(grid, start_pos, start_dir):
    rows, cols = len(grid), len(grid[0])
    energized = set()
    visited = set()
    queue = deque([(start_pos, start_dir)])

    while queue:
        (r, c), (dr, dc) = queue.popleft()
        
        if not (0 <= r < rows and 0 <= c < cols) or ((r, c), (dr, dc)) in visited:
            continue
        
        visited.add(((r, c), (dr, dc)))
        energized.add((r, c))
        
        char = grid[r][c]
        if char == '.':
            queue.append(((r + dr, c + dc), (dr, dc)))
        elif char == '/':
            queue.append(((r - dc, c - dr), (-dc, -dr)))
        elif char == '\\':
            queue.append(((r + dc, c + dr), (dc, dr)))
        elif char == '|' and dc != 0:
            queue.append(((r - 1, c), (-1, 0)))
            queue.append(((r + 1, c), (1, 0)))
        elif char == '-' and dr != 0:
            queue.append(((r, c - 1), (0, -1)))
            queue.append(((r, c + 1), (0, 1)))
        else:
            queue.append(((r + dr, c + dc), (dr, dc)))

    return len(energized)

def solve_part1(grid):
    return simulate_beam(grid, (0, 0), (0, 1))

def solve_part2(grid):
    rows, cols = len(grid), len(grid[0])
    max_energized = 0

    for r in range(rows):
        max_energized = max(max_energized, simulate_beam(grid, (r, 0), (0, 1)))
        max_energized = max(max_energized, simulate_beam(grid, (r, cols-1), (0, -1)))
    
    for c in range(cols):
        max_energized = max(max_energized, simulate_beam(grid, (0, c), (1, 0)))
        max_energized = max(max_energized, simulate_beam(grid, (rows-1, c), (-1, 0)))

    return max_energized

def main():
    grid = parse_input("input.txt")
    print(f"Part 1: {solve_part1(grid)}")
    print(f"Part 2: {solve_part2(grid)}")

if __name__ == "__main__":
    main()
