
def readInput(fileName) {
    def input = new File(fileName).text.trim()
    def matcher = (input =~ /x=(\d+)\.\.(\d+), y=(-?\d+)\.\.(-?\d+)/)
    if (matcher) {
        return [
            xMin: matcher[0][1] as int,
            xMax: matcher[0][2] as int,
            yMin: matcher[0][3] as int,
            yMax: matcher[0][4] as int
        ]
    }
    throw new IllegalArgumentException("Invalid input format")
}

def isWithinTargetArea(x, y, target) {
    return x >= target.xMin && x <= target.xMax && y >= target.yMin && y <= target.yMax
}

def simulateProbe(vx, vy, target) {
    int x = 0, y = 0
    int maxY = 0

    while (x <= target.xMax && y >= target.yMin) {
        x += vx
        y += vy
        maxY = Math.max(maxY, y)

        // Update velocities
        if (vx > 0) vx -= 1
        vy -= 1

        if (isWithinTargetArea(x, y, target)) {
            return maxY
        }
    }
    return null // Indicates that the probe did not land in the target area
}

def findMaxHeight(target) {
    int maxHeight = 0

    // Try all possible initial velocities
    for (int vx = 0; vx <= target.xMax; vx++) {
        for (int vy = target.yMin; vy <= Math.abs(target.yMin); vy++) {
            def height = simulateProbe(vx, vy, target)
            if (height != null) {
                maxHeight = Math.max(maxHeight, height)
            }
        }
    }
    return maxHeight
}

def main() {
    def target = readInput("input.txt")
    def maxHeight = findMaxHeight(target)
    println "The highest y position reached is: $maxHeight"
}

main()
