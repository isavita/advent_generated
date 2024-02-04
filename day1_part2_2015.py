with open('input.txt', 'r') as file:
    data = file.read()

floor = 0
position = 0

for char in data:
    position += 1
    if char == '(':
        floor += 1
    elif char == ')':
        floor -= 1
    if floor == -1:
        break

print(position)