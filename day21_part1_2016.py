
def apply_operation(op, password):
    fields = op.split()
    if fields[0] == "swap":
        if fields[1] == "position":
            x, y = int(fields[2]), int(fields[5])
            password = swap_position(password, x, y)
        elif fields[1] == "letter":
            x, y = fields[2], fields[5]
            password = swap_letter(password, x, y)
    elif fields[0] == "rotate":
        if fields[1] == "left":
            steps = int(fields[2])
            password = rotate_left(password, steps)
        elif fields[1] == "right":
            steps = int(fields[2])
            password = rotate_right(password, steps)
        elif fields[1] == "based":
            x = fields[6]
            password = rotate_based_on_position(password, x)
    elif fields[0] == "reverse":
        x, y = int(fields[2]), int(fields[4])
        password = reverse_positions(password, x, y)
    elif fields[0] == "move":
        x, y = int(fields[2]), int(fields[5])
        password = move_position(password, x, y)
    return password

def swap_position(password, x, y):
    if x > y:
        x, y = y, x
    return password[:x] + password[y] + password[x+1:y] + password[x] + password[y+1:]

def swap_letter(password, x, y):
    return password.replace(x, '_').replace(y, x).replace('_', y)

def rotate_left(password, steps):
    steps = steps % len(password)
    return password[steps:] + password[:steps]

def rotate_right(password, steps):
    steps = steps % len(password)
    return password[-steps:] + password[:-steps]

def rotate_based_on_position(password, x):
    index = password.find(x)
    steps = 1 + index + (1 if index >= 4 else 0)
    return rotate_right(password, steps)

def reverse_positions(password, x, y):
    if x > y:
        x, y = y, x
    return password[:x] + password[x:y+1][::-1] + password[y+1:]

def move_position(password, x, y):
    r = password[x]
    password = password[:x] + password[x+1:]
    return password[:y] + r + password[y:]

def main():
    with open("input.txt") as f:
        operations = f.read().strip().split('\n')

    password = "abcdefgh"
    for op in operations:
        password = apply_operation(op, password)

    print(password)

if __name__ == "__main__":
    main()
