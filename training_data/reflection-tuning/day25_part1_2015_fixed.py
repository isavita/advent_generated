def calculate_code(row, col):
    code = 20151125
    target_index = sum(range(row + col - 1)) + col

    for _ in range(1, target_index):
        code = (code * 252533) % 33554393

    return code

# Read input
with open('input.txt', 'r') as file:
    content = file.read().strip()

# Extract row and column from the input
import re

numbers = re.findall(r'\d+', content)
if len(numbers) >= 2:
    row, col = map(int, numbers[-2:])
else:
    raise ValueError("Could not find row and column numbers in the input")

# Calculate and print the result
result = calculate_code(row, col)
print(f"The code at row {row}, column {col} is: {result}")
