import scala.io.Source

object Solution {
  case class Coord(x: Int, y: Int) {
    def add(c2: Coord): Coord = Coord(x + c2.x, y + c2.y)
    def multiplyByScalar(s: Int): Coord = Coord(x * s, y * s)
  }

  val North = Coord(0, -1)
  val West = Coord(-1, 0)
  val South = Coord(0, 1)
  val East = Coord(1, 0)

  def abs(x: Int): Int = if (x < 0) -x else x

  def parseInput(input: Array[String]): Array[Coord] = {
    val Up = 'U'
    val Left = 'L'
    val Down = 'D'
    val Right = 'R'

    var current = Coord(0, 0)
    var vertices = Array(current)

    for (line <- input) {
      val parts = line.split(" ")
      val dirInput = parts(0)(0)
      val lengthStr = parts(1)
      var length = 0
      for (i <- 0 until lengthStr.length) {
        length = length * 10 + (lengthStr(i) - '0')
      }

      val dir = dirInput match {
        case Up => North
        case Left => West
        case Down => South
        case Right => East
      }

      current = current.add(dir.multiplyByScalar(length))
      vertices :+= current
    }

    vertices
  }

  def hexStringToInt(hexStr: String): Int = {
    Integer.parseInt(hexStr, 16)
  }

  def shoelace(vertices: Array[Coord]): Int = {
    val n = vertices.length
    var area = 0

    for (i <- 0 until n) {
      val next = (i + 1) % n
      area += vertices(i).x * vertices(next).y
      area -= vertices(i).y * vertices(next).x
    }

    area = abs(area) / 2
    area
  }

  def perimeter(vertices: Array[Coord]): Int = {
    val n = vertices.length
    var perim = 0

    for (i <- 0 until n) {
      val next = (i + 1) % n
      perim += abs(vertices(i).x - vertices(next).x) + abs(vertices(i).y - vertices(next).y)
    }

    perim
  }

  def calculatePolygonArea(vertices: Array[Coord]): Int = {
    shoelace(vertices) + perimeter(vertices) / 2 + 1
  }

  def solve(input: Array[String]): Int = {
    val vertices = parseInput(input)
    calculatePolygonArea(vertices)
  }

  def readFile(fileName: String): Array[String] = {
    Source.fromFile(fileName).getLines().toArray
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}