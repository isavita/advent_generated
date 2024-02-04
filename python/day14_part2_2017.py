with open('input.txt', 'r') as file:
    key = file.read().strip()

def knot_hash(input):
    lengths = [ord(char) for char in input] + [17, 31, 73, 47, 23]
    lst = list(range(256))
    pos = 0
    skip = 0

    for _ in range(64):
        for length in lengths:
            for i in range(length // 2):
                start = (pos + i) % len(lst)
                end = (pos + length - 1 - i) % len(lst)
                lst[start], lst[end] = lst[end], lst[start]
            pos += length + skip
            skip += 1

    dense_hash = []
    for i in range(0, len(lst), 16):
        xor = 0
        for num in lst[i:i + 16]:
            xor ^= num
        dense_hash.append(xor)

    return ''.join(f'{num:08b}' for num in dense_hash)

def get_adjacent_cells(grid, row, col):
    cells = []
    if row > 0 and grid[row - 1][col] == '1':
        cells.append((row - 1, col))
    if row < len(grid) - 1 and grid[row + 1][col] == '1':
        cells.append((row + 1, col))
    if col > 0 and grid[row][col - 1] == '1':
        cells.append((row, col - 1))
    if col < len(grid[0]) - 1 and grid[row][col + 1] == '1':
        cells.append((row, col + 1))
    return cells

def mark_region(grid, row, col):
    stack = [(row, col)]
    while stack:
        curr_row, curr_col = stack.pop()
        grid[curr_row][curr_col] = '0'
        adjacent = get_adjacent_cells(grid, curr_row, curr_col)
        stack.extend(adjacent)

# Part One
used_squares = 0
grid = []
for i in range(128):
    row_key = key + '-' + str(i)
    hash_val = knot_hash(row_key)
    used_squares += hash_val.count('1')
    grid.append(list(hash_val))

print(used_squares)

# Part Two
regions = 0
for i in range(128):
    for j in range(128):
        if grid[i][j] == '1':
            mark_region(grid, i, j)
            regions += 1

print(regions)