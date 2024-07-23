
import scala.io.Source
import scala.math.{atan2, hypot, Pi}

case class Asteroid(x: Int, y: Int, angle: Double, dist: Double)

object AsteroidVaporization {
  def main(args: Array[String]): Unit = {
    val asteroids = readAsteroids("input.txt")
    val (station, _) = findBestAsteroidLocation(asteroids)
    val vaporized = vaporizeAsteroids(asteroids, station)
    if (vaporized.length >= 200) {
      val result = vaporized(199).x * 100 + vaporized(199).y
      println(result)
    } else {
      println("Less than 200 asteroids were vaporized.")
    }
  }

  def readAsteroids(filename: String): Array[Array[Boolean]] = {
    Source.fromFile(filename).getLines().map(_.map(_ == '#').toArray).toArray
  }

  def vaporizeAsteroids(asteroids: Array[Array[Boolean]], station: (Int, Int)): Seq[Asteroid] = {
    val targets = for {
      (row, y) <- asteroids.zipWithIndex
      (isAsteroid, x) <- row.zipWithIndex if isAsteroid && (x != station._1 || y != station._2)
    } yield {
      val angle = atan2(y - station._2, x - station._1) + (if (atan2(y - station._2, x - station._1) < -Pi / 2) 2 * Pi else 0)
      val dist = hypot(x - station._1, y - station._2)
      Asteroid(x, y, angle, dist)
    }

    targets.sortBy(a => (a.angle, a.dist)).foldLeft(Seq.empty[Asteroid]) { (vaporized, target) =>
      if (vaporized.isEmpty || vaporized.last.angle != target.angle) vaporized :+ target else vaporized
    }
  }

  def findBestAsteroidLocation(asteroids: Array[Array[Boolean]]): ((Int, Int), Int) = {
    (for {
      (row, y) <- asteroids.zipWithIndex
      (isAsteroid, x) <- row.zipWithIndex if isAsteroid
    } yield {
      val count = countVisibleAsteroids(asteroids, x, y)
      ((x, y), count)
    }).maxBy(_._2)
  }

  def countVisibleAsteroids(asteroids: Array[Array[Boolean]], x: Int, y: Int): Int = {
    val angles = scala.collection.mutable.Set[Double]()
    for {
      (row, otherY) <- asteroids.zipWithIndex
      (isAsteroid, otherX) <- row.zipWithIndex if isAsteroid && !(otherX == x && otherY == y)
    } {
      angles.add(atan2(otherY - y, otherX - x))
    }
    angles.size
  }
}
