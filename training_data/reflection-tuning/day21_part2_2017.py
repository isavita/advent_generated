import itertools

def parse_rules(lines):
    rules = {}
    for line in lines:
        pattern, result = line.strip().split(' => ')
        pattern = tuple(pattern.split('/'))
        result = tuple(result.split('/'))
        rules[pattern] = result
        # Add rotations and flips
        for r in range(4):
            p = rotate(pattern)
            rules[p] = result
            rules[flip(p)] = result
            pattern = p
    return rules

def rotate(pattern):
    return tuple(''.join(row[i] for row in pattern[::-1]) for i in range(len(pattern)))

def flip(pattern):
    return tuple(row[::-1] for row in pattern)

def split_grid(grid, size):
    for i in range(0, len(grid), size):
        for j in range(0, len(grid), size):
            yield tuple(row[j:j+size] for row in grid[i:i+size])

def join_grids(grids, size):
    rows = []
    for i in range(0, len(grids), size):
        row_grids = grids[i:i+size]
        for j in range(len(row_grids[0])):
            rows.append(''.join(grid[j] for grid in row_grids))
    return tuple(rows)

def apply_rules(grid, rules):
    size = 2 if len(grid) % 2 == 0 else 3
    squares = list(split_grid(grid, size))
    new_squares = [rules[square] for square in squares]
    return join_grids(new_squares, len(grid) // size)

def count_on_pixels(grid):
    return sum(row.count('#') for row in grid)

def iterate(start_grid, rules, iterations):
    grid = start_grid
    for _ in range(iterations):
        grid = apply_rules(grid, rules)
    return grid

def solve(input_file, iterations):
    with open(input_file, 'r') as f:
        rules = parse_rules(f.readlines())
    
    start_grid = ('.#.', '..#', '###')
    final_grid = iterate(start_grid, rules, iterations)
    return count_on_pixels(final_grid)

# Part 1
print(solve('input.txt', 5))

# Part 2
print(solve('input.txt', 18))
