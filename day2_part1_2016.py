
keypad = {
    (0, 0): '1',
    (1, 0): '2',
    (2, 0): '3',
    (0, 1): '4',
    (1, 1): '5',
    (2, 1): '6',
    (0, 2): '7',
    (1, 2): '8',
    (2, 2): '9'
}

current_position = (1, 1)
code = ''

with open('input.txt', 'r') as file:
    for line in file:
        for char in line.strip():
            if char == 'U' and current_position[1] > 0:
                current_position = (current_position[0], current_position[1] - 1)
            elif char == 'D' and current_position[1] < 2:
                current_position = (current_position[0], current_position[1] + 1)
            elif char == 'L' and current_position[0] > 0:
                current_position = (current_position[0] - 1, current_position[1])
            elif char == 'R' and current_position[0] < 2:
                current_position = (current_position[0] + 1, current_position[1])
        code += keypad[current_position]

print(code)
