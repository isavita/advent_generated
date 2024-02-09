import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()
    val parts = lines[0].split(", ")
    val xRange = parts[0].substring(15).split("..")
    val yRange = parts[1].substring(2).split("..")
    val xMin = xRange[0].toInt()
    val xMax = xRange[1].toInt()
    val yMin = yRange[0].toInt()
    val yMax = yRange[1].toInt()

    val velocities = mutableMapOf<String, Boolean>()
    for (xVel in -1000..1000) {
        for (yVel in -1000..1000) {
            var xPos = 0
            var yPos = 0
            var curXVel = xVel
            var curYVel = yVel
            var inTargetArea = false
            while (true) {
                xPos += curXVel
                yPos += curYVel

                if (xPos in xMin..xMax && yPos in yMin..yMax) {
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
                val velocityKey = "$xVel,$yVel"
                velocities[velocityKey] = true
            }
        }
    }

    println(velocities.size)
}

fun isMovingAway(xPos: Int, yPos: Int, xVel: Int, yVel: Int, xMin: Int, xMax: Int, yMin: Int, yMax: Int): Boolean {
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