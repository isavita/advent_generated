import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val parts = lines[0].split(", ")
    val xRange = parts[0].substring(15).split("..")
    val yRange = parts[1].substring(2).split("..")
    val xMin = xRange[0].toInt()
    val xMax = xRange[1].toInt()
    val yMin = yRange[0].toInt()
    val yMax = yRange[1].toInt()

    var maxY = -1 shl 30
    for (xVel in -1000..1000) {
        for (yVel in -1000..1000) {
            var xPos = 0
            var yPos = 0
            var curXVel = xVel
            var curYVel = yVel
            var highestY = yPos
            while (true) {
                xPos += curXVel
                yPos += curYVel

                if (xPos in xMin..xMax && yPos in yMin..yMax) {
                    if (highestY > maxY) {
                        maxY = highestY
                    }
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
                if (yPos > highestY) {
                    highestY = yPos
                }
            }
        }
    }

    println(maxY)
}

fun isMovingAway(xPos: Int, yPos: Int, xVel: Int, yVel: Int, xMin: Int, xMax: Int, yMin: Int, yMax: Int): Boolean {
    return (xPos < xMin && xVel < 0) || (xPos > xMax && xVel > 0) || (yPos < yMin && yVel < 0)
}