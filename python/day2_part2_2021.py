
h = 0
d = 0
a = 0

with open('input.txt', 'r') as file:
    for line in file:
        command, value = line.strip().split()
        value = int(value)
        if command == 'forward':
            h += value
            d += a * value
        elif command == 'down':
            a += value
        elif command == 'up':
            a -= value

print(h * d)
