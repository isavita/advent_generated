def count_safe_tiles(first_row, num_rows):
    # Convert the input string to a binary number
    row = int(''.join('0' if c == '.' else '1' for c in first_row), 2)
    width = len(first_row)
    mask = (1 << width) - 1  # Create a bitmask for the row width
    
    safe_count = first_row.count('.')  # Count safe tiles in the first row
    
    for _ in range(num_rows - 1):  # -1 because we've already counted the first row
        # Generate the next row using bitwise operations
        row = ((row << 1) ^ (row >> 1)) & mask
        # Count safe tiles in this row
        safe_count += bin(row ^ mask).count('1')
    
    return safe_count

# Read input from file
with open('input.txt', 'r') as file:
    first_row = file.read().strip()

# Part 1
print(count_safe_tiles(first_row, 40))

# Part 2
print(count_safe_tiles(first_row, 400000))
