with open('input.txt', 'r') as f:
    jumps = [int(line.strip()) for line in f]

index = 0
steps = 0

while index >= 0 and index < len(jumps):
    offset = jumps[index]
    jumps[index] += 1 if offset < 3 else -1
    index += offset
    steps += 1

print(steps)