
with open('input.txt', 'r') as file:
    steps = int(file.read().strip())

buffer = [0]
current_pos = 0

for i in range(1, 2018):
    current_pos = (current_pos + steps) % len(buffer) + 1
    buffer.insert(current_pos, i)

print(buffer[(current_pos + 1) % len(buffer)])
