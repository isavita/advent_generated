
import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.PriorityQueue

case class Coord(x: Int, y: Int) {
  def neighbors: Seq[Coord] = {
    Seq(Coord(x + 1, y), Coord(x, y + 1)) ++
      (if (x > 0) Seq(Coord(x - 1, y)) else Seq.empty) ++
      (if (y > 0) Seq(Coord(x, y - 1)) else Seq.empty)
  }
}

object Cave {
  val geologicY = 16807
  val geologicX = 48271
  val caveModulo = 20183

  val TypeRocky = 0
  val TypeWet = 1
  val TypeNarrow = 2

  val ToolNone = 1 << 0
  val ToolTorch = 1 << 1
  val ToolGear = 1 << 2

  class Map(val target: Coord, val depth: Int) {
    private val geologicIndicesCache = mutable.Map.empty[(Int, Int), Int]
    private val erosionLevelsCache = mutable.Map.empty[(Int, Int), Int]

    def geologicIndex(x: Int, y: Int): Int = {
      geologicIndicesCache.getOrElseUpdate((x, y), {
        (x, y) match {
          case (0, 0) => 0
          case (tx, ty) if tx == target.x && ty == target.y => 0
          case (_, 0) => x * geologicY
          case (0, _) => y * geologicX
          case _ => erosionLevel(x - 1, y) * erosionLevel(x, y - 1)
        }
      })
    }

    def erosionLevel(x: Int, y: Int): Int = {
      erosionLevelsCache.getOrElseUpdate((x, y), (geologicIndex(x, y) + depth) % caveModulo)
    }

    def regionType(x: Int, y: Int): Int = erosionLevel(x, y) % 3

    def neighbors(pos: Coord, equip: Int): Seq[Item] = {
      pos.neighbors.flatMap { c =>
        val t = regionType(c.x, c.y)
        if ((equip & allowed(t)) != 0) {
          Seq(Item(c, equip, 1), Item(c, equip ^ allowed(t), 8))
        } else {
          Seq.empty
        }
      }
    }

    private def allowed(regionType: Int): Int = regionType match {
      case TypeRocky => ToolGear | ToolTorch
      case TypeWet => ToolGear | ToolNone
      case TypeNarrow => ToolTorch | ToolNone
      case _ => throw new IllegalArgumentException(s"unknown region type: $regionType")
    }
  }

  case class Item(pos: Coord, equip: Int, time: Int)

  def rescue(input: String): Int = {
    val Array(depthLine, targetLine) = input.split("\n").map(_.trim)
    val depth = depthLine.split(": ")(1).toInt
    val Array(targetX, targetY) = targetLine.split(": ")(1).split(",").map(_.toInt)

    val m = new Map(Coord(targetX, targetY), depth)

    val queue = PriorityQueue[Item]()(Ordering.by(-_.time))
    queue.enqueue(Item(Coord(0, 0), ToolTorch, 0))

    val distances = mutable.Map[(Coord, Int), Int]((Coord(0, 0), ToolTorch) -> 0)

    while (queue.nonEmpty) {
      val item = queue.dequeue()

      if (item.pos == m.target && item.equip == ToolTorch) return item.time

      for (n <- m.neighbors(item.pos, item.equip)) {
        val d = (n.pos, n.equip)
        val newTime = item.time + n.time

        if (distances.get(d).forall(_ > newTime)) {
          distances(d) = newTime
          queue.enqueue(Item(n.pos, n.equip, newTime))
        }
      }
    }
    0
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().mkString("\n")
    println(rescue(input))
  }
}
