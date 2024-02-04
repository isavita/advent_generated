
with open('input.txt', 'r') as file:
    instructions = [(line[0], int(line[1:])) for line in file.read().splitlines()]

directions = ['E', 'S', 'W', 'N']
current_dir = 'E'
x, y = 0, 0

for action, value in instructions:
    if action == 'N':
        y += value
    elif action == 'S':
        y -= value
    elif action == 'E':
        x += value
    elif action == 'W':
        x -= value
    elif action == 'L':
        current_dir = directions[(directions.index(current_dir) - value // 90) % 4]
    elif action == 'R':
        current_dir = directions[(directions.index(current_dir) + value // 90) % 4]
    elif action == 'F':
        if current_dir == 'N':
            y += value
        elif current_dir == 'S':
            y -= value
        elif current_dir == 'E':
            x += value
        elif current_dir == 'W':
            x -= value

print(abs(x) + abs(y))
