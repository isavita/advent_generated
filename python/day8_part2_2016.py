def rect(screen, width, height):
    for i in range(height):
        for j in range(width):
            screen[i][j] = '#'

def rotate_row(screen, row, by):
    screen[row] = screen[row][-by:] + screen[row][:-by]

def rotate_column(screen, col, by):
    # Creating a temporary list for the column
    temp_col = [screen[i][col] for i in range(6)]
    # Shifting the column by the specified amount
    for i in range(6):
        screen[(i + by) % 6][col] = temp_col[i]

def apply_instruction(screen, instruction):
    parts = instruction.split()
    if parts[0] == 'rect':
        width, height = map(int, parts[1].split('x'))
        rect(screen, width, height)
    elif parts[0] == 'rotate':
        idx = int(parts[2].split('=')[1])
        by = int(parts[4])
        if parts[1] == 'row':
            rotate_row(screen, idx, by)
        elif parts[1] == 'column':
            rotate_column(screen, idx, by)

def print_screen(screen):
    for row in screen:
        print(''.join(row))

# Initialize the screen
screen = [['.' for _ in range(50)] for _ in range(6)]

# Read and apply instructions from input.txt
with open('input.txt', 'r') as file:
    for instruction in file:
        apply_instruction(screen, instruction.strip())

# Print the final screen state
print_screen(screen)
