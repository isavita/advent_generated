
import numpy as np

with open("input.txt", "r") as file:
    serial = int(file.read().strip())

gridSize = 300
grid = np.zeros((gridSize, gridSize))

for y in range(gridSize):
    for x in range(gridSize):
        rackID = x + 11
        powerLevel = rackID * (y + 1)
        powerLevel += serial
        powerLevel *= rackID
        powerLevel = (powerLevel // 100) % 10
        powerLevel -= 5
        grid[y, x] = powerLevel

maxPower = -2**31
maxX, maxY = 0, 0
for y in range(gridSize - 2):
    for x in range(gridSize - 2):
        totalPower = np.sum(grid[y:y+3, x:x+3])
        if totalPower > maxPower:
            maxPower = totalPower
            maxX, maxY = x + 1, y + 1

print(f"{maxX},{maxY}")
