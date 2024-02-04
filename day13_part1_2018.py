with open('input.txt') as f:
    data = f.read().splitlines()

track = [list(row) for row in data]

directions = {'^': (0, -1), 'v': (0, 1), '<': (-1, 0), '>': (1, 0)}
turns = {'left': {'^': '<', '<': 'v', 'v': '>', '>': '^'},
         'straight': {'^': '^', '<': '<', 'v': 'v', '>': '>'},
         'right': {'^': '>', '<': '^', 'v': '<', '>': 'v'}}

carts = {}
cart_id = 1

for y in range(len(track)):
    for x in range(len(track[y])):
        if track[y][x] in directions:
            carts[cart_id] = [x, y, track[y][x], 'left']
            cart_id += 1

while True:
    for cart in sorted(carts.items(), key=lambda x: (x[1][1], x[1][0])):
        x, y, direction, next_turn = cart[1]

        del carts[cart[0]]

        dx, dy = directions[direction]
        x += dx
        y += dy

        for c in carts:
            if x == carts[c][0] and y == carts[c][1]:
                print(f'{x},{y}')
                exit()

        if track[y][x] == '+':
            if next_turn == 'left':
                direction = turns['left'][direction]
                next_turn = 'straight'
            elif next_turn == 'straight':
                next_turn = 'right'
            elif next_turn == 'right':
                direction = turns['right'][direction]
                next_turn = 'left'
        elif track[y][x] == '/':
            if direction == '>':
                direction = '^'
            elif direction == 'v':
                direction = '<'
            elif direction == '<':
                direction = 'v'
            elif direction == '^':
                direction = '>'
        elif track[y][x] == '\\':
            if direction == '>':
                direction = 'v'
            elif direction == 'v':
                direction = '>'
            elif direction == '<':
                direction = '^'
            elif direction == '^':
                direction = '<'

        carts[cart[0]] = [x, y, direction, next_turn]