
import scala.io.Source
import scala.collection.mutable

case class Coord(x: Int, y: Int) {
  def add(c: Coord): Coord = Coord(x + c.x, y + c.y)
  def multiplyByScalar(s: Int): Coord = Coord(x * s, y * s)
}

case class Grid(width: Int, height: Int, data: mutable.Map[Coord, Char])

object Main {
  val North = Coord(0, -1)
  val West = Coord(-1, 0)
  val South = Coord(0, 1)
  val East = Coord(1, 0)

  val Empty: Char = '.'
  val Rock: Char = '#'
  val Start: Char = 'S'

  def parseInput(input: Seq[String]): Grid = {
    val grid = Grid(input.head.length, input.length, mutable.Map())
    input.zipWithIndex.foreach { case (line, y) =>
      line.zipWithIndex.foreach { case (char, x) =>
        if (char != Empty) grid.data(Coord(x, y)) = char
      }
    }
    grid
  }

  def findStart(grid: Grid): Coord = {
    grid.data.collectFirst { case (coord, char) if char == Start => coord }.getOrElse(throw new Exception("No start found."))
  }

  def isInBounds(grid: Grid, coord: Coord): Boolean = {
    coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height
  }

  def neighbors4(grid: Grid, coord: Coord): Seq[Coord] = {
    Seq(North, South, East, West).map(coord.add).filter(c => isInBounds(grid, c) && grid.data.get(c).forall(_ != Rock))
  }

  def breadthFirstSearch(grid: Grid, start: Coord): Map[Coord, Int] = {
    val frontier = mutable.Queue(start)
    val reached = mutable.Set(start)
    val distances = mutable.Map(start -> 0)

    while (frontier.nonEmpty) {
      val current = frontier.dequeue()
      for (next <- neighbors4(grid, current) if !reached.contains(next)) {
        frontier.enqueue(next)
        reached.add(next)
        distances(next) = distances(current) + 1
      }
    }
    distances.toMap
  }

  def solve(input: Seq[String], numSteps: Int): Int = {
    val grid = parseInput(input)
    val start = findStart(grid)
    val distances = breadthFirstSearch(grid, start)

    distances.values.count(dist => dist <= numSteps && dist % 2 == 0)
  }

  def readFile(fileName: String): Seq[String] = {
    Source.fromFile(fileName).getLines().toSeq
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input, 64))
  }
}
