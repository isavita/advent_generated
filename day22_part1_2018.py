
import sys

def parseInput(data):
    lines = data.split("\n")
    depth = int(lines[0].split(" ")[1])
    coords = lines[1].split(" ")[1]
    parts = coords.split(",")
    x = int(parts[0])
    y = int(parts[1])
    return depth, [x, y]

def makeCaveSystem(depth, target):
    cave = [[0 for _ in range(target[0] + 1)] for _ in range(target[1] + 1)]
    for y in range(target[1] + 1):
        for x in range(target[0] + 1):
            if x == 0 and y == 0 or x == target[0] and y == target[1]:
                geologicIndex = 0
            elif y == 0:
                geologicIndex = x * 16807
            elif x == 0:
                geologicIndex = y * 48271
            else:
                geologicIndex = cave[y][x - 1] * cave[y - 1][x]
            cave[y][x] = (geologicIndex + depth) % 20183
    return cave

def calculateRiskLevel(cave, target):
    riskLevel = 0
    for y in range(target[1] + 1):
        for x in range(target[0] + 1):
            riskLevel += cave[y][x] % 3
    return riskLevel

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        data = file.read()

    depth, target = parseInput(data)
    cave = makeCaveSystem(depth, target)
    riskLevel = calculateRiskLevel(cave, target)
    print("Total Risk Level:", riskLevel)
