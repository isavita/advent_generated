
import scala.collection.mutable
import scala.io.Source

object Solution {

  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
  }

  val North: Coord = Coord(0, -1)
  val South: Coord = Coord(0, 1)
  val West: Coord = Coord(-1, 0)
  val East: Coord = Coord(1, 0)

  val Empty: Char = '.'
  val Wall: Char = '#'
  val NorthSlopes: Char = '^'
  val SouthSlopes: Char = 'v'
  val WestSlopes: Char = '<'
  val EastSlopes: Char = '>'

  val SlopeToDir: Map[Char, Coord] = Map(
    NorthSlopes -> North,
    SouthSlopes -> South,
    WestSlopes -> West,
    EastSlopes -> East
  )

  def isInBounds(grid: Map[String, Any], coord: Coord): Boolean =
    0 <= coord.x && coord.x < grid("width").asInstanceOf[Int] && 0 <= coord.y && coord.y < grid("height").asInstanceOf[Int]

  def parseInput(input: List[String]): Map[String, Any] = {
    val width = input.head.length
    val height = input.length
    val data = mutable.Map[Coord, Char]()

    for ((line, y) <- input.zipWithIndex; (char, x) <- line.zipWithIndex if char != Empty) {
      data(Coord(x, y)) = char
    }

    Map("width" -> width, "height" -> height, "data" -> data.toMap)
  }

  def isValidNeighbor(grid: Map[String, Any], coord: Coord, dir: Coord): Boolean = {
    isInBounds(grid, coord) && (!grid("data").asInstanceOf[Map[Coord, Char]].contains(coord) || grid("data").asInstanceOf[Map[Coord, Char]](coord) != Wall)
  }

  def isValidNeighborWithSlopes(grid: Map[String, Any], coord: Coord, dir: Coord): Boolean = {
    if (!isInBounds(grid, coord)) {
      false
    } else {
      grid("data").asInstanceOf[Map[Coord, Char]].get(coord) match {
        case None => true
        case Some(Wall) => false
        case Some(slope) => SlopeToDir(slope) == dir
      }
    }
  }

  def neighbors4(grid: Map[String, Any], coord: Coord, isValidNeighborFunc: (Map[String, Any], Coord, Coord) => Boolean): List[Coord] = {
    List(North, South, West, East).flatMap { dir =>
      val neighbor = coord + dir
      if (isValidNeighborFunc(grid, neighbor, dir)) Some(neighbor) else None
    }
  }
  def getGraph(grid: Map[String, Any], start: Coord, end: Coord, isValidNeighborFunc: (Map[String, Any], Coord, Coord) => Boolean): Map[String, Any] = {
    val vertices = mutable.Set(start, end)
    val edges = mutable.Map[Coord, Map[(Coord, Coord, Int), Unit]]()

    for (y <- 0 until grid("height").asInstanceOf[Int]; x <- 0 until grid("width").asInstanceOf[Int]) {
      val coord = Coord(x, y)
      if (!grid("data").asInstanceOf[Map[Coord, Char]].contains(coord)) {
        if (neighbors4(grid, coord, isValidNeighbor).length > 2) {
          vertices.add(coord)
        }
      }
    }
      vertices.foreach{
        v =>
          edges(v) = getEdgesBFS(grid,v,vertices.toSet,isValidNeighborFunc)
      }

    Map("vertices" -> vertices.toSet, "edges" -> edges.toMap)
  }

  def getEdgesBFS(grid: Map[String, Any], start: Coord, vertices: Set[Coord], isValidNeighborFunc: (Map[String, Any], Coord, Coord) => Boolean): Map[(Coord, Coord, Int), Unit] = {
    val frontier = mutable.Queue(start)
    val reached = mutable.Set(start)
    val distances = mutable.Map(start -> 0)
    val edges = mutable.Map[(Coord, Coord, Int), Unit]()

    while (frontier.nonEmpty) {
      val current = frontier.dequeue()

      if (vertices.contains(current) && current != start) {
        edges((start, current, distances(current))) = ()

      } else
        for (next <- neighbors4(grid, current, isValidNeighborFunc)) {
          if (!reached.contains(next)) {
            frontier.enqueue(next)
            reached.add(next)
            distances(next) = distances(current) + 1
          }
        }
    }

    edges.toMap
  }

    def getMaxDistanceDFS(grid: Map[String,Any], graph: Map[String,Any], current: Coord, end: Coord, seen: mutable.Set[Coord]): (Boolean,Int) = {
        if(current == end) return (true,0)
        var maxi = 0
        seen += current
        graph("edges").asInstanceOf[Map[Coord, Map[(Coord, Coord, Int), Unit]]](current).keys.foreach {
            case edge@(_,next,_) =>
                if(!seen.contains(next)) {
                    getMaxDistanceDFS(grid,graph,next,end,seen) match {
                        case (true,dist) => maxi = maxi.max(dist + edge._3)
                        case _ =>
                    }
                }

        }

        seen -= current
        if(maxi == 0) return (false,0)
        (true,maxi)

    }

  def solve(input: List[String]): Int = {
    val grid = parseInput(input)
    val start = Coord(1, 0)
    val end = Coord(grid("width").asInstanceOf[Int] - 2, grid("height").asInstanceOf[Int] - 1)

    val graph = getGraph(grid, start, end, isValidNeighborWithSlopes)

    val (_, maxDist) = getMaxDistanceDFS(grid,graph, start, end, mutable.Set[Coord]())
    maxDist
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    println(solve(input))
  }
}
