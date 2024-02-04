
with open("input.txt", "r") as file:
    startingNumbers = file.read().strip().split(",")

spoken = {}
lastSpoken = 0

for i, number in enumerate(startingNumbers):
    if i == len(startingNumbers) - 1:
        lastSpoken = int(number)
    else:
        num = int(number)
        spoken[num] = i + 1

for turn in range(len(startingNumbers) + 1, 30000001):
    if lastSpoken in spoken:
        nextNumber = turn - 1 - spoken[lastSpoken]
    else:
        nextNumber = 0
    spoken[lastSpoken] = turn - 1
    lastSpoken = nextNumber

print(lastSpoken)
