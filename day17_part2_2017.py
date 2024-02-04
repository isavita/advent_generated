
with open("input.txt", "r") as file:
    steps = int(file.read().strip())

buffer = [0]
current_pos = 0

for i in range(1, 2018):
    current_pos = (current_pos + steps) % len(buffer) + 1
    buffer.insert(current_pos, i)

print(buffer[(current_pos + 1) % len(buffer)])

# For Part Two
current_pos = 0
value_after_zero = None

for i in range(1, 50000001):
    current_pos = (current_pos + steps) % i + 1
    if current_pos == 1:
        value_after_zero = i

print(value_after_zero)
