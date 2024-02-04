
from itertools import permutations

distances = {}
locations = set()

with open('input.txt', 'r') as file:
    for line in file:
        parts = line.strip().split()
        distances[(parts[0], parts[2])] = int(parts[4])
        distances[(parts[2], parts[0])] = int(parts[4])
        locations.add(parts[0])
        locations.add(parts[2])

min_distance = float('inf')
for perm in permutations(locations):
    distance = sum(distances[(perm[i], perm[i+1])] for i in range(len(perm)-1))
    min_distance = min(min_distance, distance)

print(min_distance)
