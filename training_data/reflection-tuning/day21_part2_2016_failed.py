def scramble(password, instructions):
    for instruction in instructions:
        parts = instruction.split()
        if parts[0] == 'swap' and parts[1] == 'position':
            x, y = int(parts[2]), int(parts[5])
            password = list(password)
            password[x], password[y] = password[y], password[x]
            password = ''.join(password)
        elif parts[0] == 'swap' and parts[1] == 'letter':
            x, y = parts[2], parts[5]
            password = password.replace(x, '#').replace(y, x).replace('#', y)
        elif parts[0] == 'rotate' and parts[1] in ['left', 'right']:
            steps = int(parts[2])
            if parts[1] == 'left':
                password = password[steps:] + password[:steps]
            else:
                password = password[-steps:] + password[:-steps]
        elif parts[0] == 'rotate' and parts[1] == 'based':
            x = parts[6]
            index = password.index(x)
            steps = 1 + index + (1 if index >= 4 else 0)
            password = password[-steps:] + password[:-steps]
        elif parts[0] == 'reverse':
            x, y = int(parts[2]), int(parts[4])
            password = password[:x] + password[x:y+1][::-1] + password[y+1:]
        elif parts[0] == 'move':
            x, y = int(parts[2]), int(parts[5])
            char = password[x]
            password = password[:x] + password[x+1:]
            password = password[:y] + char + password[y:]
    return password

def unscramble(password, instructions):
    for instruction in reversed(instructions):
        parts = instruction.split()
        if parts[0] == 'swap' and parts[1] == 'position':
            x, y = int(parts[2]), int(parts[5])
            password = list(password)
            password[x], password[y] = password[y], password[x]
            password = ''.join(password)
        elif parts[0] == 'swap' and parts[1] == 'letter':
            x, y = parts[2], parts[5]
            password = password.replace(x, '#').replace(y, x).replace('#', y)
        elif parts[0] == 'rotate' and parts[1] in ['left', 'right']:
            steps = int(parts[2])
            if parts[1] == 'left':
                password = password[-steps:] + password[:-steps]
            else:
                password = password[steps:] + password[:steps]
        elif parts[0] == 'rotate' and parts[1] == 'based':
            x = parts[6]
            for i in range(len(password)):
                test = password[i:] + password[:i]
                if scramble(test, [instruction]) == password:
                    password = test
                    break
        elif parts[0] == 'reverse':
            x, y = int(parts[2]), int(parts[4])
            password = password[:x] + password[x:y+1][::-1] + password[y+1:]
        elif parts[0] == 'move':
            y, x = int(parts[2]), int(parts[5])
            char = password[x]
            password = password[:x] + password[x+1:]
            password = password[:y] + char + password[y:]
    return password

# Read instructions from file
with open('input.txt', 'r') as f:
    instructions = f.read().strip().split('\n')

# Part One
print("Part One:", scramble('abcdefgh', instructions))

# Part Two
print("Part Two:", unscramble('fbgdceah', instructions))
