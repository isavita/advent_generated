
def calculate_fuel(positions, target):
    total_fuel = 0
    for pos in positions:
        diff = abs(pos - target)
        total_fuel += diff * (diff + 1) // 2
    return total_fuel

with open('input.txt', 'r') as file:
    positions = [int(x) for x in file.read().split(',')]

min_pos = min(positions)
max_pos = max(positions)
min_fuel = float('inf')
target_pos = 0

for target in range(min_pos, max_pos + 1):
    fuel = calculate_fuel(positions, target)
    if fuel < min_fuel:
        min_fuel = fuel
        target_pos = target

print(min_fuel)
