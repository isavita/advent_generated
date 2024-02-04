
with open('input.txt', 'r') as file:
    positions = [int(x) for x in file.readline().strip().split(',')]

positions.sort()
mid = len(positions) // 2
alignment_position = positions[mid]
total_fuel = sum(abs(x - alignment_position) for x in positions)

print(total_fuel)
