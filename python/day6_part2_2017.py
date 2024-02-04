content = []
with open("input.txt", "r") as file:
    content = file.read().split()

memory_banks = list(map(int, content))

seen_states = []
cycles = 0
while memory_banks not in seen_states:
    seen_states.append(list(memory_banks))
    max_blocks = max(memory_banks)
    index = memory_banks.index(max_blocks)
    memory_banks[index] = 0
    while max_blocks > 0:
        index = (index + 1) % len(memory_banks)
        memory_banks[index] += 1
        max_blocks -= 1
    cycles += 1

print(cycles)
print(cycles - seen_states.index(memory_banks))