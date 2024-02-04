
import re

# Read input from file
with open("input.txt", "r") as file:
    data = file.read()

# Use regular expression to extract the row and column from the input.
matches = re.search(r"row (\d+), column (\d+)", data)
if not matches:
    raise ValueError("Invalid input format.")

row = int(matches.group(1))
column = int(matches.group(2))

# Calculate the position in the code sequence for the given row and column
def get_position(row, column):
    return (row + column - 2) * (row + column - 1) // 2 + column

# Calculate the code at the specified position
def get_code(position):
    start_code = 20151125
    multiplier = 252533
    modulus = 33554393

    code = start_code
    for i in range(1, position):
        code = (code * multiplier) % modulus
    return code

pos = get_position(row, column)
code = get_code(pos)

print(code)
