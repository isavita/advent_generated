
with open('input.txt') as f:
    jumps = [int(line.strip()) for line in f]

current_index = 0
steps = 0

while 0 <= current_index < len(jumps):
    next_index = current_index + jumps[current_index]
    jumps[current_index] += 1
    current_index = next_index
    steps += 1

print(steps)
