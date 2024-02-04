
def build_grid(input_lines, empty='.'):
    height = len(input_lines)
    width = len(input_lines[0])
    data = {}

    for y, line in enumerate(input_lines):
        for x, char in enumerate(line):
            if char != empty:
                data[(x, y)] = char

    return {'width': width, 'height': height, 'data': data}

def grid_to_string(grid, empty='.'):
    result = []

    for y in range(grid['height']):
        line = []
        for x in range(grid['width']):
            line.append(grid['data'].get((x, y), empty))
        result.append(''.join(line))

    return '\n'.join(result)

def get_empty_rows(grid):
    empty_rows = []

    for y in range(grid['height']):
        is_empty = True

        for x in range(grid['width']):
            if (x, y) in grid['data']:
                is_empty = False
                break

        if is_empty:
            empty_rows.append(y)

    return empty_rows

def get_empty_cols(grid):
    empty_cols = []

    for x in range(grid['width']):
        is_empty = True

        for y in range(grid['height']):
            if (x, y) in grid['data']:
                is_empty = False
                break

        if is_empty:
            empty_cols.append(x)

    return empty_cols

def calculate_offsets(empty_indexes, bound):
    offsets = [0] * bound

    for idx in empty_indexes:
        for i in range(idx + 1, len(offsets)):
            offsets[i] += 1

    return offsets

def expand_grid(grid, expansion_factor):
    empty_cols = get_empty_cols(grid)
    empty_rows = get_empty_rows(grid)
    num_lines_to_add = expansion_factor - 1

    new_width = grid['width'] + len(empty_cols) * num_lines_to_add
    new_height = grid['height'] + len(empty_rows) * num_lines_to_add
    new_data = {}

    d_xs = calculate_offsets(empty_cols, grid['width'])
    d_ys = calculate_offsets(empty_rows, grid['height'])

    for (x, y), value in grid['data'].items():
        new_x = x + d_xs[x] * num_lines_to_add
        new_y = y + d_ys[y] * num_lines_to_add
        new_data[(new_x, new_y)] = value

    return {'width': new_width, 'height': new_height, 'data': new_data}

def abs_val(x):
    return abs(x)

def calculate_length(c1, c2):
    dx = abs_val(c2[0] - c1[0])
    dy = abs_val(c2[1] - c1[1])
    return dx + dy

def solve(input_lines):
    grid = build_grid(input_lines)

    expanded_grid = expand_grid(grid, 2)

    res = 0
    already_seen = set()
    for coord1 in expanded_grid['data']:
        for coord2 in already_seen:
            length = calculate_length(coord1, coord2)
            res += length
        already_seen.add(coord1)

    return res

def read_file(file_name):
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]

def main():
    input_lines = read_file("input.txt")
    print(solve(input_lines))

if __name__ == "__main__":
    main()
