
def file = new File("input.txt")
def rebootSteps = []

file.eachLine { line ->
    if (line) {
        rebootSteps.add(parseRebootStep(line))
    }
}

def minCoord = -50
def maxCoord = 50
def cubeGrid = createCubeGrid(minCoord, maxCoord)
executeRebootSteps(cubeGrid, rebootSteps)
def onCubes = countOnCubes(cubeGrid)

println(onCubes)

def parseRebootStep(line) {
    def parts = line.split(" ")
    
    def action = parts[0]
    parts = parts[1].split(",")
    def xRange = parts[0][2..-1].split("\\.\\.")
    def yRange = parts[1][2..-1].split("\\.\\.")
    def zRange = parts[2][2..-1].split("\\.\\.")
    
    def xStart = xRange[0] as Integer
    def xEnd = xRange[1] as Integer
    def yStart = yRange[0] as Integer
    def yEnd = yRange[1] as Integer
    def zStart = zRange[0] as Integer
    def zEnd = zRange[1] as Integer
    
    return [action, xStart, xEnd, yStart, yEnd, zStart, zEnd]
}

def createCubeGrid(minCoord, maxCoord) {
    def gridSize = maxCoord - minCoord + 1
    def grid = new boolean[gridSize][gridSize][gridSize]
    
    return grid
}

def executeRebootSteps(cubeGrid, rebootSteps) {
    rebootSteps.each { step ->
        if (!(step[1] >= -50 && step[2] <= 50 && step[3] >= -50 && step[4] <= 50 && step[5] >= -50 && step[6] <= 50)) {
            return
        }
        for (int x = step[1]; x <= step[2]; x++) {
            for (int y = step[3]; y <= step[4]; y++) {
                for (int z = step[5]; z <= step[6]; z++) {
                    cubeGrid[x + 50][y + 50][z + 50] = step[0] == "on"
                }
            }
        }
    }
}

def countOnCubes(cubeGrid) {
    def count = 0
    
    cubeGrid.each { x ->
        x.each { y ->
            y.each { z ->
                if (z) {
                    count++
                }
            }
        }
    }
    
    return count
}
