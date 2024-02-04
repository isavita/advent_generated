
from collections import defaultdict

replacements = defaultdict(list)
medicine = ""

with open("input.txt", "r") as file:
    for line in file:
        line = line.strip()
        if " => " in line:
            key, value = line.split(" => ")
            replacements[key].append(value)
        elif line:
            medicine = line

distinct_molecules = set()

for key, values in replacements.items():
    for i in range(len(medicine)):
        if medicine[i:i+len(key)] == key:
            for value in values:
                new_molecule = medicine[:i] + value + medicine[i+len(key):]
                distinct_molecules.add(new_molecule)

print(len(distinct_molecules))
