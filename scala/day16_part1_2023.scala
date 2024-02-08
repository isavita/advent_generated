object Solution extends App {

  case class Coord(x: Int, y: Int) {
    def add(c: Coord): Coord = Coord(x + c.x, y + c.y)
    def rotate90: Coord = Coord(y, -x)
    def rotateNeg90: Coord = Coord(-y, x)
    def isInBounds(grid: Grid): Boolean = x >= 0 && x < grid.width && y >= 0 && y < grid.height
  }

  case class Grid(width: Int, height: Int, data: Map[Coord, Char])

  case class Beam(origin: Coord, dir: Coord)

  val empty: Char = '.'
  val ascendingMirror: Char = '/'
  val descendingMirror: Char = '\\'
  val verticalSplitter: Char = '|'
  val horizontalSplitter: Char = '-'

  val north = Coord(0, -1)
  val west = Coord(-1, 0)
  val south = Coord(0, 1)
  val east = Coord(1, 0)

  def nextBeam(grid: Grid, beam: Beam): List[Beam] = {
    var beams: List[Beam] = List()
    val char = grid.data.getOrElse(beam.origin, empty)

    if (!grid.data.contains(beam.origin)) {
      val newBeam = Beam(beam.origin.add(beam.dir), beam.dir)
      beams = beams :+ newBeam
    } else {
      char match {
        case `ascendingMirror` =>
          val newDir = if (beam.dir == north || beam.dir == south) beam.dir.rotateNeg90 else beam.dir.rotate90
          val newBeam = Beam(beam.origin.add(newDir), newDir)
          beams = beams :+ newBeam

        case `descendingMirror` =>
          val newDir = if (beam.dir == north || beam.dir == south) beam.dir.rotate90 else beam.dir.rotateNeg90
          val newBeam = Beam(beam.origin.add(newDir), newDir)
          beams = beams :+ newBeam

        case `verticalSplitter` if beam.dir == east || beam.dir == west =>
          val newDir1 = beam.dir.rotate90
          val newBeam1 = Beam(beam.origin.add(newDir1), newDir1)
          val newDir2 = beam.dir.rotateNeg90
          val newBeam2 = Beam(beam.origin.add(newDir2), newDir2)
          beams = beams :+ newBeam1 :+ newBeam2

        case `horizontalSplitter` if beam.dir == north || beam.dir == south =>
          val newDir1 = beam.dir.rotate90
          val newBeam1 = Beam(beam.origin.add(newDir1), newDir1)
          val newDir2 = beam.dir.rotateNeg90
          val newBeam2 = Beam(beam.origin.add(newDir2), newDir2)
          beams = beams :+ newBeam1 :+ newBeam2

        case _ =>
          val newBeam = Beam(beam.origin.add(beam.dir), beam.dir)
          beams = beams :+ newBeam
      }
    }

    beams
  }

  def calculatePropagation(grid: Grid, start: Beam): Set[Beam] = {
    var alreadySeen: Set[Beam] = Set()
    var toExplore: List[Beam] = List(start)

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
    var beamGrid = Grid(grid.width, grid.height, grid.data ++ alreadySeen.filter(b => !grid.data.contains(b.origin)).map(b =>
      b.origin -> {
        if (grid.data.contains(b.origin)) '2'
        else b.dir match {
          case `north` => '^'
          case `east` => '>'
          case `south` => 'v'
          case `west` => '<'
        }
      }
    ))

    beamGrid
  }

  def calculateEnergization(alreadySeen: Set[Beam]): Set[Coord] = {
    alreadySeen.map(_.origin)
  }

  def getBorder(grid: Grid): List[Beam] = {
    var border: List[Beam] = List()

    for (x <- 0 until grid.width) {
      var coord = Coord(x, 0)
      var beam = Beam(coord, south)
      border = border :+ beam

      coord = Coord(x, grid.height - 1)
      beam = Beam(coord, north)
      border = border :+ beam
    }

    for (y <- 0 until grid.height) {
      var coord = Coord(0, y)
      var beam = Beam(coord, east)
      border = border :+ beam

      coord = Coord(grid.width - 1, y)
      beam = Beam(coord, west)
      border = border :+ beam
    }

    border
  }

  def solve(input: List[String]): Int = {
    val grid = Grid(input(0).length, input.length, input.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.collect {
        case (char, x) if char != empty => Coord(x, y) -> char
      }
    }.toMap)

    val start = Beam(Coord(0, 0), east)
    val alreadySeen = calculatePropagation(grid, start)
    val alreadyEnergized = calculateEnergization(alreadySeen)

    alreadyEnergized.size
  }

  def readFile(fileName: String): List[String] = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = try source.getLines().toList finally source.close()
    lines
  }

  val input = readFile("input.txt")
  println(solve(input))
}