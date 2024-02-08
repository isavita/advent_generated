
import scala.io.Source
import scala.math._

object Main extends App {
  val asteroids = readAsteroids("input.txt")
  val maxCount = findBestAsteroidLocation(asteroids)
  println(maxCount)

  def readAsteroids(filename: String): Array[Array[Boolean]] = {
    val lines = Source.fromFile(filename).getLines().toArray
    lines.map(line => line.map(_ == '#').toArray)
  }

  def findBestAsteroidLocation(asteroids: Array[Array[Boolean]]): Int = {
    var maxCount = 0
    for (y <- asteroids.indices) {
      for (x <- asteroids(y).indices) {
        if (asteroids(y)(x)) {
          val count = countVisibleAsteroids(asteroids, x, y)
          if (count > maxCount) {
            maxCount = count
          }
        }
      }
    }
    maxCount
  }

  def countVisibleAsteroids(asteroids: Array[Array[Boolean]], x: Int, y: Int): Int = {
    val angles = collection.mutable.Set[Double]()
    for {
      otherY <- asteroids.indices
      otherX <- asteroids(otherY).indices
      if asteroids(otherY)(otherX) && !(otherX == x && otherY == y)
    } {
      val angle = atan2(otherY - y, otherX - x)
      angles.add(angle)
    }
    angles.size
  }
}
