
from itertools import permutations

happiness = {}
guests = set()

with open('input.txt') as f:
    for line in f:
        line = line.strip().split()
        person1 = line[0]
        person2 = line[-1][:-1]
        change = int(line[3])
        if line[2] == 'gain':
            happiness[(person1, person2)] = change
        else:
            happiness[(person1, person2)] = -change
        guests.add(person1)

guests.add('You')

max_happiness = 0
for arrangement in permutations(guests):
    total_happiness = 0
    for i in range(len(arrangement)):
        person1 = arrangement[i]
        person2 = arrangement[(i + 1) % len(arrangement)]
        total_happiness += happiness.get((person1, person2), 0)
        total_happiness += happiness.get((person2, person1), 0)
    max_happiness = max(max_happiness, total_happiness)

print(max_happiness)
