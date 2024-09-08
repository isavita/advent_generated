def rotate(pattern):
    return [''.join(row) for row in zip(*pattern[::-1])]

def flip(pattern):
    return [row[::-1] for row in pattern]

def all_orientations(pattern):
    orientations = []
    for _ in range(4):
        orientations.append(pattern)
        orientations.append(flip(pattern))
        pattern = rotate(pattern)
    return orientations

def parse_rule(rule):
    input_pattern, output_pattern = rule.split(' => ')
    return input_pattern.split('/'), output_pattern.split('/')

def match_rule(square, rules):
    square_str = '/'.join(square)
    for orientation in all_orientations(square):
        orientation_str = '/'.join(orientation)
        if orientation_str in rules:
            return len(rules[orientation_str]), rules[orientation_str]
    print(f"No match found for square: {square_str}")  # Debugging output
    return None

def enhance(grid, rules):
    size = len(grid)
    if size % 2 == 0:
        new_size = size // 2 * 3
        step = 2
    else:
        new_size = size // 3 * 4
        step = 3

    new_grid = [['.' for _ in range(new_size)] for _ in range(new_size)]

    for i in range(0, size, step):
        for j in range(0, size, step):
            square = [row[j:j+step] for row in grid[i:i+step]]
            result = match_rule(square, rules)
            if result is None:
                raise ValueError(f"No matching rule found for square at ({i}, {j})")
            new_size, new_square = result
            
            ni, nj = i // step * new_size, j // step * new_size
            for x in range(new_size):
                for y in range(new_size):
                    new_grid[ni+x][nj+y] = new_square[x][y]

    return new_grid

def solve(rules_input):
    rules = {}
    for rule in rules_input.split('\n'):
        input_pattern, output_pattern = parse_rule(rule)
        rules['/'.join(input_pattern)] = output_pattern

    grid = ['.#.', '..#', '###']
    
    for _ in range(5):
        grid = enhance(grid, rules)

    return sum(row.count('#') for row in grid)

# Example usage:
rules = """../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#"""
print(solve(rules))
