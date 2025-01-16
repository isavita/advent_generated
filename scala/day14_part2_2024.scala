
import scala.io.Source
import scala.util.matching.Regex

case class Robot(x: Int, y: Int, vx: Int, vy: Int)

object Main extends App {
  val sizeX = 101
  val sizeY = 103

  def mod(a: Int, b: Int): Int = (a % b + b) % b

  def parseLine(line: String): Robot = {
    val re: Regex = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r
    line match {
      case re(x, y, vx, vy) => Robot(x.toInt, y.toInt, vx.toInt, vy.toInt)
    }
  }

  def moveRobots(robots: Array[Robot], sizeX: Int, sizeY: Int): Unit = {
    for (i <- robots.indices) {
      robots(i) = robots(i).copy(x = mod(robots(i).x + robots(i).vx, sizeX), y = mod(robots(i).y + robots(i).vy, sizeY))
    }
  }

  def countQuadrants(robots: Array[Robot], sizeX: Int, sizeY: Int): Array[Int] = {
    val counts = Array.fill(4)(0)
    val centerX = sizeX / 2
    val centerY = sizeY / 2

    for (robot <- robots) {
      val x = robot.x
      val y = robot.y
      if (x < centerX) {
        if (y < centerY) counts(0) += 1 else if (y > centerY) counts(1) += 1
      } else if (x > centerX) {
        if (y < centerY) counts(2) += 1 else if (y > centerY) counts(3) += 1
      }
    }
    counts
  }

  def hasNoOverlaps(robots: Array[Robot]): Boolean = {
    val positionMap = collection.mutable.Map.empty[(Int, Int), Boolean]
    for (robot <- robots) {
      val pos = (robot.x, robot.y)
      if (positionMap.contains(pos)) return false
      positionMap(pos) = true
    }
    true
  }

  def drawGrid(robots: Array[Robot], sizeX: Int, sizeY: Int): Unit = {
    val gridMap = collection.mutable.Map.empty[(Int, Int), Boolean]
    for (robot <- robots) gridMap((robot.x, robot.y)) = true

    for (y <- 0 until sizeY) {
      val line = (0 until sizeX).map(x => if (gridMap.contains((x, y))) "#" else ".").mkString
      println(line)
    }
  }

  val robots = Source.fromFile("input.txt").getLines().map(parseLine).toArray

  val robotsPart1 = robots.clone()
  for (_ <- 0 until 100) moveRobots(robotsPart1, sizeX, sizeY)
  val counts = countQuadrants(robotsPart1, sizeX, sizeY)
  val safetyFactor = counts.product
  println(s"Part 1 - Safety Factor after 100 seconds: $safetyFactor")

  val robotsPart2 = robots.clone()
  var seconds = 0
  while (!hasNoOverlaps(robotsPart2)) {
    moveRobots(robotsPart2, sizeX, sizeY)
    seconds += 1
    if (seconds > 1000000) {
      println("Exceeded maximum iterations without finding a unique position configuration.")
      sys.exit(1)
    }
  }
  println(s"Part 2 - Fewest seconds to display Easter egg: $seconds")
  println("Final positions of robots:")
  drawGrid(robotsPart2, sizeX, sizeY)
}
