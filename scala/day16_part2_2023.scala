
import scala.io.Source

case class Coord(x: Int, y: Int) {
  def add(c: Coord): Coord = Coord(x + c.x, y + c.y)
  def rotate90(): Coord = Coord(y, -x)
  def rotateNeg90(): Coord = Coord(-y, x)
  def isInBounds(grid: Grid): Boolean = x >= 0 && x < grid.width && y >= 0 && y < grid.height
}

case class Beam(origin: Coord, dir: Coord)

case class Grid(width: Int, height: Int, data: Map[Coord, Char])

object Main {
  val Empty: Char = '.'
  val AscendingMirror: Char = '/'
  val DescendingMirror: Char = '\\'
  val VerticalSplitter: Char = '|'
  val HorizontalSplitter: Char = '-'

  val North: Coord = Coord(0, -1)
  val West: Coord = Coord(-1, 0)
  val South: Coord = Coord(0, 1)
  val East: Coord = Coord(1, 0)

  def rotate90(coord: Coord): Coord = Coord(coord.y, -coord.x)
  def rotateNeg90(coord: Coord): Coord = Coord(-coord.y, coord.x)

  def buildGrid(input: Array[String]): Grid = {
    val data = input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.filter { case (char, _) => char != Empty }.map { case (char, x) =>
        Coord(x, y) -> char
      }
    }.toMap

    Grid(input(0).length, input.length, data)
  }

  def nextBeam(grid: Grid, beam: Beam): List[Beam] = {
    val char = grid.data.getOrElse(beam.origin, Empty)

    char match {
      case AscendingMirror =>
        val newDir = if (beam.dir == North || beam.dir == South) beam.dir.rotateNeg90() else beam.dir.rotate90()
        List(Beam(beam.origin.add(newDir), newDir))

      case DescendingMirror =>
        val newDir = if (beam.dir == North || beam.dir == South) beam.dir.rotate90() else beam.dir.rotateNeg90()
        List(Beam(beam.origin.add(newDir), newDir))

      case VerticalSplitter if beam.dir == East || beam.dir == West =>
        val newDir1 = beam.dir.rotate90()
        val newDir2 = beam.dir.rotateNeg90()
        List(Beam(beam.origin.add(newDir1), newDir1), Beam(beam.origin.add(newDir2), newDir2))

      case HorizontalSplitter if beam.dir == North || beam.dir == South =>
        val newDir1 = beam.dir.rotate90()
        val newDir2 = beam.dir.rotateNeg90()
        List(Beam(beam.origin.add(newDir1), newDir1), Beam(beam.origin.add(newDir2), newDir2))

      case _ =>
        List(Beam(beam.origin.add(beam.dir), beam.dir))
    }
  }

  def calculatePropagation(grid: Grid, start: Beam): Set[Beam] = {
    var alreadySeen = Set.empty[Beam]
    var toExplore = List(start)

    while (toExplore.nonEmpty) {
      val beam = toExplore.head
      toExplore = toExplore.tail

      if (beam.origin.isInBounds(grid) && !alreadySeen.contains(beam)) {
        alreadySeen += beam
        toExplore ++= nextBeam(grid, beam)
      }
    }

    alreadySeen
  }

  def buildBeamGrid(grid: Grid, alreadySeen: Set[Beam]): Grid = {
    var data = grid.data ++ alreadySeen.filter(beam => !grid.data.contains(beam.origin)).map { beam =>
      beam.origin -> (if (grid.data.contains(beam.origin)) '2' else beam.dir match {
        case North => '^'
        case East => '>'
        case South => 'v'
        case West => '<'
      })
    }

    Grid(grid.width, grid.height, data)
  }

  def calculateEnergization(alreadySeen: Set[Beam]): Set[Coord] = alreadySeen.map(_.origin)

  def getBorder(grid: Grid): List[Beam] = {
    val topBorder = (0 until grid.width).map(x => Beam(Coord(x, 0), South)).toList
    val bottomBorder = (0 until grid.width).map(x => Beam(Coord(x, grid.height - 1), North)).toList
    val leftBorder = (0 until grid.height).map(y => Beam(Coord(0, y), East)).toList
    val rightBorder = (0 until grid.height).map(y => Beam(Coord(grid.width - 1, y), West)).toList

    topBorder ++ bottomBorder ++ leftBorder ++ rightBorder
  }

  def solve(input: Array[String]): Int = {
    val grid = buildGrid(input)
    val starts = getBorder(grid)

    starts.foldLeft(0) { (res, start) =>
      val alreadySeen = calculatePropagation(grid, start)
      val alreadyEnergized = calculateEnergization(alreadySeen)
      val energy = alreadyEnergized.size
      if (energy > res) energy else res
    }
  }

  def readFile(fileName: String): Array[String] = Source.fromFile(fileName).getLines.toArray

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
