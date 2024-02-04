from itertools import permutations

distances = {}
locations = set()

with open('input.txt', 'r') as file:
    for line in file:
        line = line.strip().split(' ')
        distances[(line[0], line[2])] = int(line[4])
        distances[(line[2], line[0])] = int(line[4])
        locations.add(line[0])
        locations.add(line[2])

shortest_distance = float('inf')
longest_distance = 0

for perm in permutations(locations):
    total_distance = 0
    for i in range(len(perm) - 1):
        total_distance += distances[(perm[i], perm[i+1])]
    shortest_distance = min(shortest_distance, total_distance)
    longest_distance = max(longest_distance, total_distance)

print(shortest_distance)
print(longest_distance)