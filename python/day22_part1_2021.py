import sys

class RebootStep:
    def __init__(self, action, xStart, xEnd, yStart, yEnd, zStart, zEnd):
        self.action = action
        self.xStart = xStart
        self.xEnd = xEnd
        self.yStart = yStart
        self.yEnd = yEnd
        self.zStart = zStart
        self.zEnd = zEnd

def parse_reboot_step(line):
    parts = line.split(" ")

    action = parts[0]
    parts = parts[1].split(",")
    xRange = parts[0][2:].split("..")
    yRange = parts[1][2:].split("..")
    zRange = parts[2][2:].split("..")

    xStart = int(xRange[0])
    xEnd = int(xRange[1])
    yStart = int(yRange[0])
    yEnd = int(yRange[1])
    zStart = int(zRange[0])
    zEnd = int(zRange[1])

    return RebootStep(action, xStart, xEnd, yStart, yEnd, zStart, zEnd)

def create_cube_grid(minCoord, maxCoord):
    gridSize = maxCoord - minCoord + 1
    grid = [[[False for _ in range(gridSize)] for _ in range(gridSize)] for _ in range(gridSize)]

    return grid

def execute_reboot_steps(cubeGrid, rebootSteps):
    for step in rebootSteps:
        if not (step.xStart >= -50 and step.xEnd <= 50 and step.yStart >= -50 and step.yEnd <= 50 and step.zStart >= -50 and step.zEnd <= 50):
            continue
        for x in range(step.xStart, step.xEnd + 1):
            for y in range(step.yStart, step.yEnd + 1):
                for z in range(step.zStart, step.zEnd + 1):
                    cubeGrid[x + 50][y + 50][z + 50] = step.action == "on"

def count_on_cubes(cubeGrid):
    count = 0

    for i in range(len(cubeGrid)):
        for j in range(len(cubeGrid[i])):
            for k in range(len(cubeGrid[i][j])):
                if cubeGrid[i][j][k]:
                    count += 1

    return count

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        lines = file.readlines()

    rebootSteps = []
    for line in lines:
        if line.strip() == "":
            continue
        step = parse_reboot_step(line.strip())
        rebootSteps.append(step)

    minCoord, maxCoord = -50, 50
    cubeGrid = create_cube_grid(minCoord, maxCoord)
    execute_reboot_steps(cubeGrid, rebootSteps)
    onCubes = count_on_cubes(cubeGrid)

    print(onCubes)