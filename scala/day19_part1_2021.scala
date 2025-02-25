
import scala.io.Source
import scala.collection.mutable
import scala.annotation.tailrec

object Day19 {

  case class Point3D(x: Int, y: Int, z: Int) {
    def -(other: Point3D): Point3D = Point3D(x - other.x, y - other.y, z - other.z)
    def +(other: Point3D): Point3D = Point3D(x + other.x, y + other.y, z + other.z)
    def manhattanDistance(other: Point3D): Int =
      math.abs(x - other.x) + math.abs(y - other.y) + math.abs(z - other.z)

    def allRotations: Seq[Point3D] = Seq(
      Point3D(x, y, z), Point3D(x, -z, y), Point3D(x, -y, -z), Point3D(x, z, -y),
      Point3D(-x, -y, z), Point3D(-x, -z, -y), Point3D(-x, y, -z), Point3D(-x, z, y),
      Point3D(y, -x, z), Point3D(y, -z, -x), Point3D(y, x, -z), Point3D(y, z, x),
      Point3D(-y, x, z), Point3D(-y, -z, x), Point3D(-y, -x, -z), Point3D(-y, z, -x),
      Point3D(z, y, -x), Point3D(z, x, y), Point3D(z, -y, x), Point3D(z, -x, -y),
      Point3D(-z, -y, -x), Point3D(-z, x, -y), Point3D(-z, y, x), Point3D(-z, -x, y)
    )
  }


  def parseInput(filename: String): Seq[Seq[Point3D]] = {
    val lines = Source.fromFile(filename).getLines().toList
    val scanners = mutable.ListBuffer[Seq[Point3D]]()
    val currentScanner = mutable.ListBuffer[Point3D]()

    for (line <- lines) {
      if (line.startsWith("---")) {
        // Start of a new scanner
        if (currentScanner.nonEmpty) {
          scanners += currentScanner.toSeq
          currentScanner.clear()
        }
      } else if (line.nonEmpty) {
        // Beacon coordinates
        val coords = line.split(",").map(_.toInt)
        currentScanner += Point3D(coords(0), coords(1), coords(2))
      }
    }
    if (currentScanner.nonEmpty) {
      scanners += currentScanner.toSeq
    }
    scanners.toSeq
  }


  def findOverlap(scanner1: Seq[Point3D], scanner2: Seq[Point3D]): Option[(Seq[Point3D], Point3D)] = {
    for (rotatedScanner2 <- scanner2.map(_.allRotations).transpose) {
      for (p1 <- scanner1) {
        for (p2 <- rotatedScanner2) {
          val offset = p1 - p2
          val translatedScanner2 = rotatedScanner2.map(_ + offset)
          val overlapCount = scanner1.intersect(translatedScanner2).length
          if (overlapCount >= 12) {
            return Some((translatedScanner2, offset))
          }
        }
      }
    }
    None
  }


  @tailrec
  def assembleMap(
                   remainingScanners: Seq[Seq[Point3D]],
                   knownBeacons: Set[Point3D],
                   scannerPositions: List[Point3D]
                 ): (Set[Point3D], List[Point3D]) = {
    if (remainingScanners.isEmpty) {
      (knownBeacons, scannerPositions)
    } else {
      val (nextRemaining, nextKnown, nextPositions) = remainingScanners.foldLeft(
        (List[Seq[Point3D]](), knownBeacons, scannerPositions)
      ) { case ((remainingAcc, knownAcc, positionsAcc), scanner) =>
        findOverlap(knownAcc.toSeq, scanner) match {
          case Some((translatedScanner, offset)) =>
            (remainingAcc, knownAcc ++ translatedScanner, offset :: positionsAcc)
          case None =>
            (scanner :: remainingAcc, knownAcc, positionsAcc)
        }
      }
      assembleMap(nextRemaining, nextKnown, nextPositions)
    }
  }


  def main(args: Array[String]): Unit = {
    val scanners = parseInput("input.txt")

    val (allBeacons, scannerPositions) = assembleMap(scanners.tail, scanners.head.toSet, List(Point3D(0, 0, 0)))

    println(allBeacons.size)

  }
}
