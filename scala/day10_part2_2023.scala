
import scala.io.Source
import scala.collection.mutable

object Main {

  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
    def -(other: Coord): Coord = Coord(x - other.x, y - other.y)
    def unary_- : Coord = Coord(-x, -y)
  }

  type Tile = Char
  type Pipe = Set[Coord]
  case class Grid(width: Int, height: Int, data: Map[Coord, Tile])

  val Undefined = Coord(0, 0)
  val Top = Coord(0, -1)
  val Right = Coord(1, 0)
  val Bottom = Coord(0, 1)
  val Left = Coord(-1, 0)

  val Empty: Tile = '.'
  val Start: Tile = 'S'
  val Vertical: Tile = '|'
  val Horizontal: Tile = '-'
  val TopLeftCorner: Tile = 'J'
  val TopRightCorner: Tile = 'L'
  val BottomLeftCorner: Tile = '7'
  val BottomRightCorner: Tile = 'F'
  val Enclosed: Tile = 'X'

  val VerticalPipe: Pipe = Set(Top, Bottom)
  val HorizontalPipe: Pipe = Set(Left, Right)
  val TopLeftCornerPipe: Pipe = Set(Top, Left)
  val TopRightCornerPipe: Pipe = Set(Top, Right)
  val BottomLeftCornerPipe: Pipe = Set(Bottom, Left)
  val BottomRightCornerPipe: Pipe = Set(Bottom, Right)

  val TileToPipe: Map[Tile, Pipe] = Map(
    Vertical -> VerticalPipe,
    Horizontal -> HorizontalPipe,
    TopLeftCorner -> TopLeftCornerPipe,
    TopRightCorner -> TopRightCornerPipe,
    BottomLeftCorner -> BottomLeftCornerPipe,
    BottomRightCorner -> BottomRightCornerPipe
  )

  def getPipeFromTile(tile: Tile): Pipe = TileToPipe.getOrElse(tile, Set.empty)

  def getTileFromPipe(pipe: Pipe): Tile =
    TileToPipe.find(_._2 == pipe).map(_._1).getOrElse(Empty)

  def buildGrid(input: List[String]): Grid = {
    val data = (for {
      (line, y) <- input.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char != Empty
    } yield Coord(x, y) -> char).toMap
    Grid(input.head.length, input.length, data)
  }

  def findStart(grid: Grid): Coord =
    grid.data.find(_._2 == Start).map(_._1).getOrElse(Coord(0,0))

  def getPipeFromNeighbors(c: Coord, grid: Grid): Pipe = {
    val possibleNeighbors = Map(
      Top -> (c + Top),
      Right -> (c + Right),
      Bottom -> (c + Bottom),
      Left -> (c + Left)
    )

    possibleNeighbors.foldLeft(Set.empty[Coord]) {
      case (pipe, (dir, neighborCoord)) =>
        grid.data.get(neighborCoord) match {
          case Some(neighborTile) =>
            val neighborPipe = getPipeFromTile(neighborTile)
            if (neighborPipe.contains(-dir)) pipe + dir else pipe
          case None => pipe
        }
    }
  }

  def pathFinding(start: Coord, grid: Grid): List[Coord] = {
    val path = mutable.ListBuffer(start)
    val startPipe = getPipeFromNeighbors(start, grid)

    var previousDir = startPipe.head
    var current = start + previousDir

    while (current != start) {
      path += current
      val currentPipe = getPipeFromTile(grid.data(current))
      previousDir = currentPipe.find(_ != -previousDir).get
      current = current + previousDir
    }
    path.toList
  }

  def getPathGrid(grid: Grid, path: List[Coord], empty: Tile): Grid = {
    val newData = path.map(coord => coord -> grid.data(coord)).toMap
    val start = path.head
    val startTile = getTileFromPipe(getPipeFromNeighbors(start, grid))
    Grid(grid.width, grid.height, newData + (start -> startTile))
  }

  def isInside(c: Coord, grid: Grid, empty: Tile): Boolean = {
    if (grid.data.contains(c)) return false

    var startPipe: Tile = empty
    var numPipeOnLeft = 0
    for (x <- 0 until c.x) {
      val coord = Coord(x, c.y)
      grid.data.get(coord) match {
        case Some(Vertical) => numPipeOnLeft += 1
        case Some(TopRightCorner) => startPipe = TopRightCorner
        case Some(BottomRightCorner) => startPipe = BottomRightCorner
        case Some(TopLeftCorner) =>
          if (startPipe == BottomRightCorner) {
            startPipe = empty
            numPipeOnLeft += 1
          } else if (startPipe == TopRightCorner) {
            startPipe = empty
          }
        case Some(BottomLeftCorner) =>
          if (startPipe == TopRightCorner) {
            startPipe = empty
            numPipeOnLeft += 1
          } else if (startPipe == BottomRightCorner) {
            startPipe = empty
          }
        case _ =>
      }
    }
    numPipeOnLeft % 2 == 1
  }

  def solve(input: List[String]): Int = {
    val grid = buildGrid(input)
    val start = findStart(grid)
    val path = pathFinding(start, grid)
    val pathGrid = getPathGrid(grid, path, Empty)

    (for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      c = Coord(x, y)
      if isInside(c, pathGrid, Empty)
    } yield 1).sum
  }

  def readFile(fileName: String): List[String] = {
    val source = Source.fromFile(fileName)
    try source.getLines().toList
    finally source.close()
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
