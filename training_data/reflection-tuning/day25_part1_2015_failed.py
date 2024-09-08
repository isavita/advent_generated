def next_code(prev_code):
    return (prev_code * 252533) % 33554393

def calculate_position(row, col):
    return (row + col - 1) * (row + col - 2) // 2 + col

def find_code(target_row, target_col):
    target_position = calculate_position(target_row, target_col)
    code = 20151125
    for _ in range(1, target_position):
        code = next_code(code)
    return code

# Read input
with open('input.txt', 'r') as file:
    content = file.read().strip()
    row, col = map(int, content.split())

# Calculate and print the result
result = find_code(row, col)
print(result)
