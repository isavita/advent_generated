with open("input.txt", "r") as file:
    memory_banks = list(map(int, file.read().split()))

seen_configs = []
cycles = 0

while memory_banks not in seen_configs:
    seen_configs.append(memory_banks[:])
    max_blocks = max(memory_banks)
    idx = memory_banks.index(max_blocks)
    memory_banks[idx] = 0
    while max_blocks > 0:
        idx = (idx + 1) % len(memory_banks)
        memory_banks[idx] += 1
        max_blocks -= 1
    cycles += 1

print(cycles)