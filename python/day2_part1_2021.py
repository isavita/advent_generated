
h, d = 0, 0
with open('input.txt', 'r') as file:
    for line in file:
        action, value = line.strip().split()
        value = int(value)
        if action == 'forward':
            h += value
        elif action == 'down':
            d += value
        elif action == 'up':
            d -= value

result = h * d
print(result)
