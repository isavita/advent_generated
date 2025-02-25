
import scala.io.Source
import scala.collection.mutable

object Day19 {

  case class Point3D(x: Int, y: Int, z: Int) {
    def -(other: Point3D): Point3D = Point3D(x - other.x, y - other.y, z - other.z)
    def +(other: Point3D): Point3D = Point3D(x + other.x, y + other.y, z + other.z)
    def manhattanDistance(other: Point3D): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

    def rotations: Seq[Point3D] = Seq(
      Point3D(x, y, z), Point3D(x, -z, y), Point3D(x, -y, -z), Point3D(x, z, -y),
      Point3D(-x, -y, z), Point3D(-x, -z, -y), Point3D(-x, y, -z), Point3D(-x, z, y),
      Point3D(y, -x, z), Point3D(y, -z, -x), Point3D(y, x, -z), Point3D(y, z, x),
      Point3D(-y, x, z), Point3D(-y, -z, x), Point3D(-y, -x, -z), Point3D(-y, z, -x),
      Point3D(z, y, -x), Point3D(z, x, y), Point3D(z, -y, x), Point3D(z, -x, -y),
      Point3D(-z, y, x), Point3D(-z, -x, y), Point3D(-z, -y, -x), Point3D(-z, x, -y)
    )
  }

  def parseInput(filename: String): Seq[Seq[Point3D]] = {
    val lines = Source.fromFile(filename).getLines().toList
    val scannerData = mutable.ListBuffer[Seq[Point3D]]()
    val currentScanner = mutable.ListBuffer[Point3D]()

    for (line <- lines) {
      if (line.startsWith("---")) {
        if (currentScanner.nonEmpty) {
          scannerData += currentScanner.toSeq
          currentScanner.clear()
        }
      } else if (line.nonEmpty) {
        val coords = line.split(",").map(_.toInt)
        currentScanner += Point3D(coords(0), coords(1), coords(2))
      }
    }
    scannerData += currentScanner.toSeq
    scannerData.toSeq
  }


  def findOverlap(scanner1: Seq[Point3D], scanner2: Seq[Point3D]): Option[(Seq[Point3D], Point3D)] = {
    for (rotation <- 0 until 24) {
      val rotatedScanner2 = scanner2.map(_.rotations(rotation))
      val diffCounts = mutable.Map[Point3D, Int]()

      for (p1 <- scanner1) {
        for (p2 <- rotatedScanner2) {
          val diff = p1 - p2
          diffCounts(diff) = diffCounts.getOrElse(diff, 0) + 1
        }
      }

      val (translation, count) = diffCounts.maxByOption(_._2).getOrElse((Point3D(0, 0, 0), 0))
      if (count >= 12) {
        val translatedScanner2 = rotatedScanner2.map(_ + translation)
        return Some((translatedScanner2, translation))
      }
    }
    None
  }

  def solve(scanners: Seq[Seq[Point3D]]): (Set[Point3D], Set[Point3D]) = {
    val allBeacons = mutable.Set[Point3D]() ++ scanners.head
    val scannerPositions = mutable.Set[Point3D](Point3D(0, 0, 0))
    val remainingScanners = mutable.Queue[Seq[Point3D]](scanners.tail: _*)

    while (remainingScanners.nonEmpty) {
      val scanner = remainingScanners.dequeue()
      findOverlap(allBeacons.toSeq, scanner) match {
        case Some((translatedScanner, scannerPos)) =>
          allBeacons ++= translatedScanner
          scannerPositions += scannerPos
        case None =>
          remainingScanners.enqueue(scanner)
      }
    }
    (allBeacons.toSet, scannerPositions.toSet)
  }


  def main(args: Array[String]): Unit = {
    val scanners = parseInput("input.txt")

    val (allBeacons, scannerPositions) = solve(scanners)

    println(allBeacons.size)

    val maxDistance = scannerPositions.toSeq.combinations(2)
      .map { case Seq(a, b) => a.manhattanDistance(b) }
      .max
    println(maxDistance)
  }
}

