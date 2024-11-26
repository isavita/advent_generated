
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

  var maxY = Int.MinValue
  for {
    xVel <- -1000 to 1000
    yVel <- -1000 to 1000
  } {
    var xPos = 0
    var yPos = 0
    var curXVel = xVel
    var curYVel = yVel
    var highestY = 0
    var reachedTarget = false

    while (!reachedTarget && !(xPos > xMax || (yPos < yMin && curYVel < 0))) {
      xPos += curXVel
      yPos += curYVel
      highestY = highestY max yPos

      if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
        maxY = maxY max highestY
        reachedTarget = true
      }

      curXVel = if (curXVel > 0) curXVel - 1 else if (curXVel < 0) curXVel + 1 else 0
      curYVel -= 1
    }
  }
  println(maxY)
}
