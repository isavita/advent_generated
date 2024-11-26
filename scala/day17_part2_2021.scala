
import scala.io.Source

object Solution extends App {
  val source = Source.fromFile("input.txt")
  val line = source.getLines().next()
  source.close()

  val parts = line.split(", ")
  val xRange = parts(0).substring(15).split("\\.\\.").map(_.toInt)
  val yRange = parts(1).substring(2).split("\\.\\.").map(_.toInt)
  val (xMin, xMax) = (xRange(0), xRange(1))
  val (yMin, yMax) = (yRange(0), yRange(1))

  val velocities = collection.mutable.Set.empty[(Int, Int)]
  for {
    xVel <- -1000 to 1000
    yVel <- -1000 to 1000
  } {
    var xPos = 0
    var yPos = 0
    var curXVel = xVel
    var curYVel = yVel
    var inTargetArea = false

    while (!inTargetArea && !isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
      xPos += curXVel
      yPos += curYVel

      if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
        inTargetArea = true
      }

      curXVel = if (curXVel > 0) curXVel - 1 else if (curXVel < 0) curXVel + 1 else 0
      curYVel -= 1
    }

    if (inTargetArea) {
      velocities.add((xVel, yVel))
    }
  }

  println(velocities.size)
}

def isMovingAway(xPos: Int, yPos: Int, xVel: Int, yVel: Int, xMin: Int, xMax: Int, yMin: Int, yMax: Int): Boolean = {
  (xPos < xMin && xVel <= 0) || (xPos > xMax && xVel >= 0) || (yPos < yMin && yVel <= 0)
}
