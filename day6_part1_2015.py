lights = [[0 for _ in range(1000)] for _ in range(1000)]

def turn_on(x1, y1, x2, y2):
    for i in range(x1, x2+1):
        for j in range(y1, y2+1):
            lights[i][j] = 1

def turn_off(x1, y1, x2, y2):
    for i in range(x1, x2+1):
        for j in range(y1, y2+1):
            lights[i][j] = 0

def toggle(x1, y1, x2, y2):
    for i in range(x1, x2+1):
        for j in range(y1, y2+1):
            lights[i][j] = 1 - lights[i][j]

with open('input.txt', 'r') as file:
    for line in file:
        parts = line.split()
        if parts[0] == 'turn':
            x1, y1 = map(int, parts[2].split(','))
            x2, y2 = map(int, parts[4].split(','))
            if parts[1] == 'on':
                turn_on(x1, y1, x2, y2)
            elif parts[1] == 'off':
                turn_off(x1, y1, x2, y2)
        elif parts[0] == 'toggle':
            x1, y1 = map(int, parts[1].split(','))
            x2, y2 = map(int, parts[3].split(','))
            toggle(x1, y1, x2, y2)

total_lights_on = sum([sum(row) for row in lights])
print(total_lights_on)