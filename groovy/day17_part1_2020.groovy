
import groovy.transform.Memoized

class ConwayCubes {

    static int[][][] initialGrid

    static int cycles = 6

    static int minX, maxX, minY, maxY, minZ, maxZ


    static void main(String[] args) {
        initialGrid = readInput("input.txt")
        minX = 0; maxX = initialGrid.length -1; minY = 0; maxY = initialGrid[0].length -1; minZ = 0; maxZ = initialGrid[0][0].length -1

        for (int cycle = 0; cycle < cycles; cycle++) {
            int[][][] nextGrid = simulateCycle()
            initialGrid = nextGrid
            updateBounds()
        }

        int activeCubes = countActiveCubes()
        println "Active cubes after ${cycles} cycles: ${activeCubes}"
    }

    @Memoized
    static int[][][] readInput(String filename) {
        def lines = new File(filename).readLines()
        int xSize = lines.size()
        int ySize = lines[0].size()
        int[][][] grid = new int[xSize][ySize][1]
        for (int x = 0; x < xSize; x++) {
            for (int y = 0; y < ySize; y++) {
                grid[x][y][0] = lines[x][y] == '#' ? 1 : 0
            }
        }
        return grid
    }

    static int[][][] simulateCycle() {
        int[][][] nextGrid = new int[maxX - minX + 3][maxY - minY + 3][maxZ - minZ + 3]
        for (int x = minX -1; x <= maxX + 1; x++) {
            for (int y = minY -1; y <= maxY + 1; y++) {
                for (int z = minZ -1; z <= maxZ + 1; z++) {
                    int activeNeighbors = countActiveNeighbors(x, y, z)
                    int currentState = getState(x,y,z)
                    if (currentState == 1 && (activeNeighbors == 2 || activeNeighbors == 3)) {
                        nextGrid[x - minX + 1][y - minY + 1][z - minZ + 1] = 1
                    } else if (currentState == 0 && activeNeighbors == 3) {
                        nextGrid[x - minX + 1][y - minY + 1][z - minZ + 1] = 1
                    } else {
                        nextGrid[x - minX + 1][y - minY + 1][z - minZ + 1] = 0
                    }
                }
            }
        }
        return nextGrid
    }

    static int countActiveNeighbors(int x, int y, int z) {
        int count = 0
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                for (int dz = -1; dz <= 1; dz++) {
                    if (dx == 0 && dy == 0 && dz == 0) continue
                    count += getState(x + dx, y + dy, z + dz)
                }
            }
        }
        return count
    }

    static int getState(int x, int y, int z) {
        if (x < minX || x > maxX || y < minY || y > maxY || z < minZ || z > maxZ) return 0
        return initialGrid[x - minX][y - minY][z - minZ]
    }

    static int countActiveCubes() {
        int count = 0
        for (int x = 0; x < initialGrid.length; x++) {
            for (int y = 0; y < initialGrid[0].length; y++) {
                for (int z = 0; z < initialGrid[0][0].length; z++) {
                    count += initialGrid[x][y][z]
                }
            }
        }
        return count
    }

    static void updateBounds(){
        minX--; maxX++; minY--; maxY++; minZ--; maxZ++;
    }
}
