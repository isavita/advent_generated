def generate_next_row(current_row):
    next_row = ""
    for i in range(len(current_row)):
        left = current_row[i-1] if i > 0 else '.'
        center = current_row[i]
        right = current_row[i+1] if i < len(current_row) - 1 else '.'
        
        if (left == '^' and center == '^' and right == '.') or \
           (center == '^' and right == '^' and left == '.') or \
           (left == '^' and center == '.' and right == '.') or \
           (right == '^' and center == '.' and left == '.'):
            next_row += '^'
        else:
            next_row += '.'
    return next_row

def count_safe_tiles(initial_row, total_rows):
    current_row = initial_row
    safe_count = current_row.count('.')
    
    for _ in range(total_rows - 1):
        current_row = generate_next_row(current_row)
        safe_count += current_row.count('.')
    
    return safe_count

# Read input from file
with open('input.txt', 'r') as file:
    initial_row = file.read().strip()

# Calculate and print the result
result = count_safe_tiles(initial_row, 40)
print(result)
