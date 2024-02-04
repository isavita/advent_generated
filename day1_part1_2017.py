
with open("input.txt", "r") as file:
    data = file.read().strip()

total = sum(int(data[i]) for i in range(len(data)) if data[i] == data[(i+1) % len(data)])

print(total)
