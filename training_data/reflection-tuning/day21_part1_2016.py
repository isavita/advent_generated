def swap_positions(s, x, y):
    s = list(s)
    s[x], s[y] = s[y], s[x]
    return ''.join(s)

def swap_letters(s, x, y):
    return s.translate(str.maketrans(x+y, y+x))

def rotate(s, direction, steps):
    steps %= len(s)
    if direction == 'right':
        return s[-steps:] + s[:-steps]
    return s[steps:] + s[:steps]

def rotate_based_on_letter(s, x):
    index = s.index(x)
    steps = 1 + index + (1 if index >= 4 else 0)
    return rotate(s, 'right', steps)

def reverse_positions(s, x, y):
    return s[:x] + s[x:y+1][::-1] + s[y+1:]

def move_position(s, x, y):
    char = s[x]
    s = s[:x] + s[x+1:]
    return s[:y] + char + s[y:]

password = "abcdefgh"

with open("input.txt", "r") as file:
    for line in file:
        parts = line.strip().split()
        if parts[0] == "swap" and parts[1] == "position":
            password = swap_positions(password, int(parts[2]), int(parts[5]))
        elif parts[0] == "swap" and parts[1] == "letter":
            password = swap_letters(password, parts[2], parts[5])
        elif parts[0] == "rotate" and parts[1] in ["left", "right"]:
            password = rotate(password, parts[1], int(parts[2]))
        elif parts[0] == "rotate" and parts[1] == "based":
            password = rotate_based_on_letter(password, parts[6])
        elif parts[0] == "reverse":
            password = reverse_positions(password, int(parts[2]), int(parts[4]))
        elif parts[0] == "move":
            password = move_position(password, int(parts[2]), int(parts[5]))

print(password)
