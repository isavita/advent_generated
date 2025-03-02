
class FuelCell {

    static int calculatePowerLevel(int x, int y, int serialNumber) {
        int rackId = x + 10
        int powerLevel = rackId * y
        powerLevel += serialNumber
        powerLevel *= rackId
        powerLevel = (powerLevel / 100) % 10
        powerLevel -= 5
        return powerLevel
    }

    static def findLargestTotalPower(int serialNumber) {
        int[][] grid = new int[301][301]
        for (int x = 1; x <= 300; x++) {
            for (int y = 1; y <= 300; y++) {
                grid[x][y] = calculatePowerLevel(x, y, serialNumber)
            }
        }

        int maxPower = Integer.MIN_VALUE
        int bestX = 0
        int bestY = 0

        for (int x = 1; x <= 298; x++) {
            for (int y = 1; y <= 298; y++) {
                int totalPower = 0
                for (int i = 0; i < 3; i++) {
                    for (int j = 0; j < 3; j++) {
                        totalPower += grid[x + i][y + j]
                    }
                }
                if (totalPower > maxPower) {
                    maxPower = totalPower
                    bestX = x
                    bestY = y
                }
            }
        }

        return [bestX, bestY]
    }
    static def findLargestTotalPowerAnySize(int serialNumber) {
        int[][] grid = new int[301][301]
        for (int x = 1; x <= 300; x++) {
            for (int y = 1; y <= 300; y++) {
                grid[x][y] = calculatePowerLevel(x, y, serialNumber)
            }
        }

        // Use a summed-area table for efficient calculation
        int[][] summedAreaTable = new int[301][301]
        for (int x = 1; x <= 300; x++) {
            for (int y = 1; y <= 300; y++) {
                summedAreaTable[x][y] = grid[x][y] + summedAreaTable[x - 1][y] +
                                        summedAreaTable[x][y - 1] - summedAreaTable[x - 1][y - 1]
            }
        }
        
        int maxPower = Integer.MIN_VALUE
        int bestX = 0
        int bestY = 0
        int bestSize = 0

        for (int size = 1; size <= 300; size++) {
            for (int x = 1; x <= 301 - size; x++) {
                for (int y = 1; y <= 301 - size; y++) {
                    int totalPower = summedAreaTable[x + size - 1][y + size - 1] -
                                     summedAreaTable[x - 1][y + size - 1] -
                                     summedAreaTable[x + size - 1][y - 1] +
                                     summedAreaTable[x - 1][y - 1]
                    
                    if (totalPower > maxPower) {
                        maxPower = totalPower
                        bestX = x
                        bestY = y
                        bestSize = size
                    }
                }
            }
        }

        return [bestX, bestY, bestSize]
    }


    static void main(String[] args) {
        // Read serial number from input.txt
        File inputFile = new File("input.txt")
        if (!inputFile.exists()) {
            println "Error: input.txt not found."
            return
        }
        int serialNumber = inputFile.text.trim().toInteger()
        
        // Part 1
        def result1 = findLargestTotalPower(serialNumber)
        println "Part 1: ${result1[0]},${result1[1]}"

        // Part 2
        def result2 = findLargestTotalPowerAnySize(serialNumber)
        println "Part 2: ${result2[0]},${result2[1]},${result2[2]}"
    }
}
