
import scala.io.Source

object Main extends App {
  val Neighbors8 = List((0, 1), (0, -1), (1, 0), (-1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1))

  case class Part(xmin: Int, xmax: Int, y: Int, n: Int) {
    def valid(grid: Map[(Int, Int), Char]): Boolean = {
      for (x <- xmin to xmax) {
        for (neighbor <- Neighbors8) {
          val (dx, dy) = neighbor
          val c = grid.get((x + dx, y + dy))
          if (c.isDefined && c.get != '.' && (c.get < '0' || c.get > '9')) {
            return true
          }
        }
      }
      false
    }
  }

  val input = Source.fromFile("input.txt").getLines().toList

  val grid = collection.mutable.Map[(Int, Int), Char]()
  var parts = List[Part]()
  var curr: Option[Part] = None

  for ((line, y) <- input.zipWithIndex) {
    if (curr.isDefined) {
      parts :+= curr.get
      curr = None
    }
    for ((c, x) <- line.zipWithIndex) {
      grid((x, y)) = c
      if (c >= '0' && c <= '9') {
        if (curr.isEmpty) {
          curr = Some(Part(x, x, y, c.toInt - '0'))
        } else {
          curr = curr.map(p => p.copy(n = p.n * 10 + (c.toInt - '0'), xmax = x))
        }
      } else if (curr.isDefined) {
        parts :+= curr.get
        curr = None
      }
    }
  }

  val partsGrid = collection.mutable.Map[(Int, Int), Int]()
  for ((p, i) <- parts.zipWithIndex) {
    for (x <- p.xmin to p.xmax) {
      partsGrid((x, p.y)) = i
    }
  }

  var sum = 0
  for ((p, c) <- grid) {
    if (c == '*') {
      val neighborParts = collection.mutable.Set[Int]()
      for (neighbor <- Neighbors8) {
        val (dx, dy) = neighbor
        if (partsGrid.isDefinedAt((p._1 + dx, p._2 + dy))) {
          neighborParts += partsGrid((p._1 + dx, p._2 + dy))
        }
      }
      if (neighborParts.size == 2) {
        val prod = neighborParts.map(parts).map(_.n).product
        sum += prod
      }
    }
  }
  println(sum)
}
