with open('input.txt', 'r') as file:
    polymer = file.read().strip()

def react(polymer):
    i = 0
    while i < len(polymer) - 1:
        if polymer[i] != polymer[i + 1] and polymer[i].lower() == polymer[i + 1].lower():
            polymer = polymer[:i] + polymer[i + 2:]
            i = max(0, i - 1)
        else:
            i += 1
    return polymer

reacted_polymer = react(polymer)
print(len(reacted_polymer))

units = set(polymer.lower())
min_length = len(polymer)
for unit in units:
    filtered_polymer = polymer.replace(unit, '').replace(unit.upper(), '')
    reacted_length = len(react(filtered_polymer))
    if reacted_length < min_length:
        min_length = reacted_length

print(min_length)