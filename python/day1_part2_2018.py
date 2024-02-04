
with open('input.txt', 'r') as file:
    changes = [int(line.strip()) for line in file]

frequency = 0
seen = set()
seen.add(frequency)

duplicate_found = False
while not duplicate_found:
    for change in changes:
        frequency += change
        if frequency in seen:
            print(frequency)
            duplicate_found = True
            break
        seen.add(frequency)
