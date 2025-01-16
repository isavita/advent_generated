
import scala.collection.mutable
import scala.io.Source

case class Coord(x: Int, y: Int) {
  def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
}

case class Grid(width: Int, height: Int, data: Map[Coord, Char]) {
  def isInBounds(coord: Coord): Boolean = coord.x >= 0 && coord.x < width && coord.y >= 0 && coord.y < height
}

object Directions {
  val North = Coord(0, -1)
  val South = Coord(0, 1)
  val West = Coord(-1, 0)
  val East = Coord(1, 0)
}

object Slopes {
  val NorthSlopes = '^'
  val SouthSlopes = 'v'
  val WestSlopes = '<'
  val EastSlopes = '>'
}

val SlopeToDir = Map(
  Slopes.NorthSlopes -> Directions.North,
  Slopes.SouthSlopes -> Directions.South,
  Slopes.WestSlopes -> Directions.West,
  Slopes.EastSlopes -> Directions.East
)

case class Edge(start: Coord, end: Coord, weight: Int)

case class Graph(vertices: Set[Coord], edges: Map[Coord, Set[Edge]])

object GridSolver {
  def parseInput(input: List[String]): Grid = {
    val width = input.head.length
    val height = input.length
    val data = input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.collect {
        case (char, x) if char != '.' => Coord(x, y) -> char
      }
    }.toMap
    Grid(width, height, data)
  }

  def isValidNeighbor(grid: Grid, coord: Coord, dir: Coord): Boolean = {
    grid.isInBounds(coord) && grid.data.get(coord).forall(_ != '#')
  }

  def isValidNeighborWithSlopes(grid: Grid, coord: Coord, dir: Coord): Boolean = {
    grid.isInBounds(coord) && grid.data.get(coord).forall {
      case '#' => false
      case slope => SlopeToDir.get(slope).contains(dir)
      case _ => true
    }
  }

  def neighbors4(grid: Grid, coord: Coord, isValidNeighborFunc: (Grid, Coord, Coord) => Boolean): List[Coord] = {
    List(Directions.North, Directions.South, Directions.West, Directions.East).flatMap { dir =>
      val neighbor = coord + dir
      if (isValidNeighborFunc(grid, neighbor, dir)) Some(neighbor) else None
    }
  }

  def getGraph(grid: Grid, start: Coord, end: Coord, isValidNeighborFunc: (Grid, Coord, Coord) => Boolean): Graph = {
    val vertices = mutable.Set(start, end)
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      coord = Coord(x, y)
      if !grid.data.contains(coord) && neighbors4(grid, coord, isValidNeighborFunc).length > 2
    } vertices += coord

    val edges = vertices.map { start =>
      start -> getEdgesBFS(grid, start, vertices.toSet, isValidNeighborFunc)
    }.toMap

    Graph(vertices.toSet, edges)
  }

  def getEdgesBFS(grid: Grid, start: Coord, vertices: Set[Coord], isValidNeighborFunc: (Grid, Coord, Coord) => Boolean): Set[Edge] = {
    val frontier = mutable.Queue(start)
    val reached = mutable.Set(start)
    val distances = mutable.Map(start -> 0)
    val edges = mutable.Set[Edge]()

    while (frontier.nonEmpty) {
      val current = frontier.dequeue()
      if (vertices.contains(current) && current != start) {
        edges += Edge(start, current, distances(current))
      } else {
        for (next <- neighbors4(grid, current, isValidNeighborFunc) if !reached.contains(next)) {
          frontier.enqueue(next)
          reached += next
          distances(next) = distances(current) + 1
        }
      }
    }

    edges.toSet
  }

  def getMaxDistanceDFS(grid: Grid, graph: Graph, current: Coord, end: Coord, seen: mutable.Set[Coord]): (Boolean, Int) = {
    if (current == end) return (true, 0)

    var maxi = 0
    seen += current
    for (edge <- graph.edges(current) if !seen.contains(edge.end)) {
      val (isValid, dist) = getMaxDistanceDFS(grid, graph, edge.end, end, seen)
      if (isValid) maxi = math.max(maxi, dist + edge.weight)
    }
    seen -= current

    if (maxi == 0) (false, 0) else (true, maxi)
  }

  def solve(input: List[String]): Int = {
    val grid = parseInput(input)
    val start = Coord(1, 0)
    val end = Coord(grid.width - 2, grid.height - 1)
    val graph = getGraph(grid, start, end, isValidNeighbor)
    val (_, maxDist) = getMaxDistanceDFS(grid, graph, start, end, mutable.Set())
    maxDist
  }

  def readFile(fileName: String): List[String] = {
    Source.fromFile(fileName).getLines().toList
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
