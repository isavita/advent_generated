
with open("input.txt", "r") as file:
    line = file.readline().strip()
    parts = line.split(", ")
    xRange = parts[0][15:].split("..")
    yRange = parts[1][2:].split("..")
    xMin, xMax = int(xRange[0]), int(xRange[1])
    yMin, yMax = int(yRange[0]), int(yRange[1])

velocities = {}
for xVel in range(-1000, 1001):
    for yVel in range(-1000, 1001):
        xPos, yPos = 0, 0
        curXVel, curYVel = xVel, yVel
        inTargetArea = False
        while True:
            xPos += curXVel
            yPos += curYVel

            if xMin <= xPos <= xMax and yMin <= yPos <= yMax:
                inTargetArea = True
                break

            if xPos < xMin and curXVel < 0:
                break
            if xPos > xMax and curXVel > 0:
                break
            if yPos < yMin and curYVel < 0:
                break

            if curXVel > 0:
                curXVel -= 1
            elif curXVel < 0:
                curXVel += 1
            curYVel -= 1

        if inTargetArea:
            velocityKey = f"{xVel},{yVel}"
            velocities[velocityKey] = True

print(len(velocities))
