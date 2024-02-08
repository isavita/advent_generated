
def file = new File("input.txt")
def lines = file.readLines()
def parts = lines[0].split(", ")
def xRange = parts[0].substring(15).split("\\.\\.")
def yRange = parts[1].substring(2).split("\\.\\.")
def xMin = Integer.parseInt(xRange[0])
def xMax = Integer.parseInt(xRange[1])
def yMin = Integer.parseInt(yRange[0])
def yMax = Integer.parseInt(yRange[1])

def velocities = [:]
for (def xVel = -1000; xVel <= 1000; xVel++) {
    for (def yVel = -1000; yVel <= 1000; yVel++) {
        def xPos = 0
        def yPos = 0
        def curXVel = xVel
        def curYVel = yVel
        def inTargetArea = false
        while (true) {
            xPos += curXVel
            yPos += curYVel

            if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                inTargetArea = true
                break
            }

            if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
                break
            }

            if (curXVel > 0) {
                curXVel--
            } else if (curXVel < 0) {
                curXVel++
            }

            curYVel--
        }

        if (inTargetArea) {
            def velocityKey = "$xVel,$yVel"
            velocities[velocityKey] = true
        }
    }
}

println velocities.size()

def isMovingAway(xPos, yPos, xVel, yVel, xMin, xMax, yMin, yMax) {
    if (xPos < xMin && xVel < 0) {
        return true
    }
    if (xPos > xMax && xVel > 0) {
        return true
    }
    if (yPos < yMin && yVel < 0) {
        return true
    }
    return false
}
