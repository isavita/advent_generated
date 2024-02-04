with open("input.txt") as f:
    data = f.read().strip()

total = 0
length = len(data)

for i in range(length):
    if data[i] == data[(i + length//2) % length]:
        total += int(data[i])

print(total)