input_file = open("input.txt", "r")
instructions = input_file.read().split("\n")

keypad1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
keypad2 = [[0, 0, 1, 0, 0], [0, 2, 3, 4, 0], [5, 6, 7, 8, 9], [0, 'A', 'B', 'C', 0], [0, 0, 'D', 0, 0]]

def get_code(instructions, keypad):
    code = ""
    x, y = 1, 1
    
    for instruction in instructions:
        for move in instruction:
            if move == 'U' and x > 0 and keypad[x-1][y] != 0:
                x -= 1
            elif move == 'D' and x < len(keypad)-1 and keypad[x+1][y] != 0:
                x += 1
            elif move == 'L' and y > 0 and keypad[x][y-1] != 0:
                y -= 1
            elif move == 'R' and y < len(keypad[0])-1 and keypad[x][y+1] != 0:
                y += 1
        
        code += str(keypad[x][y])
    
    return code

print(get_code(instructions, keypad1))  # Output for Part One
print(get_code(instructions, keypad2))  # Output for Part Two