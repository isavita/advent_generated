
def is_trap(left, center, right):
    if left == '^' and center == '^' and right == '.':
        return '^'
    if center == '^' and right == '^' and left == '.':
        return '^'
    if left == '^' and center == '.' and right == '.':
        return '^'
    if right == '^' and center == '.' and left == '.':
        return '^'
    return '.'

def count_safe_tiles(row):
    return row.count('.')

with open('input.txt', 'r') as file:
    first_row = file.readline().strip()

rows = [first_row]
safe_tiles = count_safe_tiles(first_row)

for _ in range(39):
    next_row = ''
    for i in range(len(first_row)):
        left = '.' if i == 0 else rows[-1][i-1]
        center = rows[-1][i]
        right = '.' if i == len(first_row)-1 else rows[-1][i+1]
        next_row += is_trap(left, center, right)
    rows.append(next_row)
    safe_tiles += count_safe_tiles(next_row)

print(safe_tiles)
