
totalCups = 1000000
totalMoves = 10000000

with open("input.txt", "r") as file:
    input = file.readline().strip()

cups = [0] * (totalCups + 1)
lastCup = 0

for i, char in enumerate(input):
    cup = int(char)
    if i > 0:
        cups[lastCup] = cup
    lastCup = cup

for i in range(len(input) + 1, totalCups + 1):
    cups[lastCup] = i
    lastCup = i
cups[lastCup] = int(input[0])

currentCup = int(input[0])
for i in range(totalMoves):
    pickup1 = cups[currentCup]
    pickup2 = cups[pickup1]
    pickup3 = cups[pickup2]

    cups[currentCup] = cups[pickup3]

    destinationCup = currentCup - 1 if currentCup > 1 else totalCups
    while destinationCup == pickup1 or destinationCup == pickup2 or destinationCup == pickup3:
        destinationCup = destinationCup - 1 if destinationCup > 1 else totalCups

    cups[pickup3] = cups[destinationCup]
    cups[destinationCup] = pickup1

    currentCup = cups[currentCup]

cup1 = cups[1]
cup2 = cups[cup1]
print(cup1 * cup2)
