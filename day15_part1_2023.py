
with open('input.txt', 'r') as f:
    data = f.read().strip()

steps = data.split(',')

total = 0
for step in steps:
    current_value = 0
    for char in step:
        current_value += ord(char)
        current_value *= 17
        current_value %= 256
    total += current_value

print(total)
