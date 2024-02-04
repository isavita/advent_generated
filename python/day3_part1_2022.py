
with open('input.txt', 'r') as file:
    data = file.read().splitlines()

total = 0
for line in data:
    first_half = line[:len(line)//2]
    second_half = line[len(line)//2:]
    common_items = set(first_half) & set(second_half)
    total += sum(ord(item.lower()) - 96 if item.islower() else ord(item) - 38 for item in common_items)

print(total)
