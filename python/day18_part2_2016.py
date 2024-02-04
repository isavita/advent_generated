
def read_first_row(filename):
    with open(filename, 'r') as file:
        return file.readline().strip()

def count_safe_tiles(first_row, total_rows):
    current_row = first_row
    safe_count = current_row.count('.')

    for i in range(1, total_rows):
        next_row = ""
        for j in range(len(current_row)):
            if is_trap(j-1, j, j+1, current_row):
                next_row += "^"
            else:
                next_row += "."
                safe_count += 1
        current_row = next_row
    return safe_count

def is_trap(left, center, right, row):
    l = row[left] if 0 <= left < len(row) else '.'
    c = row[center]
    r = row[right] if 0 <= right < len(row) else '.'

    return (l == '^' and c == '^' and r == '.') or \
           (c == '^' and r == '^' and l == '.') or \
           (l == '^' and c == '.' and r == '.') or \
           (r == '^' and c == '.' and l == '.')

def solution():
    first_row = read_first_row("input.txt")
    safe_tiles_count = count_safe_tiles(first_row, 400000)
    print(safe_tiles_count)

solution()
