
import scala.io.Source

object Solution {
  case class P(x: Int, y: Int)
  case class Elf(var pos: P, var moving: Boolean = false, var nextPos: P = null)

  val N: Int = 1
  val E: Int = 3
  val S: Int = 5
  val W: Int = 7

  var elvesMap: Map[P, Unit] = Map.empty
  var elves: List[Elf] = List.empty
  var order: Array[Int] = Array(N, S, W, E)
  var currDir: Int = 0
  val dirs: Array[P] = Array(
    P(-1, -1), P(-1, 0), P(-1, 1),
    P(0, 1), P(1, 1), P(1, 0),
    P(1, -1), P(0, -1)
  )

  def aroundAllEmpty(e: Elf): Boolean = {
    dirs.forall(d => !elvesMap.contains(P(e.pos.x + d.x, e.pos.y + d.y)))
  }

  def elfInDirection(e: Elf, wannaGo: Int): Boolean = {
    (-1 to 1).exists { j =>
      val dxy = dirs((wannaGo + j + 8) % 8)
      elvesMap.contains(P(e.pos.x + dxy.x, e.pos.y + dxy.y))
    }
  }

  def run(): Boolean = {
    val proposes: collection.mutable.Map[P, Int] = collection.mutable.Map.empty.withDefaultValue(0)

    for (e <- elves) {
      if (!aroundAllEmpty(e)) {
        var moved = false
        for (i <- 0 until 4 if !moved) {
          val dir = order((currDir + i) % 4)
          if (!elfInDirection(e, dir)) {
            val dxy = dirs(dir)
            val dest = P(e.pos.x + dxy.x, e.pos.y + dxy.y)
            proposes(dest) += 1
            e.nextPos = dest
            e.moving = true
            moved = true
          }
        }
      }
    }

    var someoneMoved = false
    for (e <- elves) {
      if (e.moving) {
        if (proposes(e.nextPos) == 1) {
          someoneMoved = true
          elvesMap -= e.pos
          elvesMap += (e.nextPos -> ())
          e.pos = e.nextPos
        }
        e.moving = false
      }
    }

    currDir = (currDir + 1) % 4
    someoneMoved
  }

  def main(args: Array[String]): Unit = {
    parse()

    var i = 0
    while (run()) {
      i += 1
    }
    println(i + 1)
  }

  def parse(): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    for ((line, row) <- lines.zipWithIndex) {
      for ((char, col) <- line.zipWithIndex) {
        if (char == '#') {
          val p = P(row, col)
          elvesMap += (p -> ())
          elves :+= Elf(p)
        }
      }
    }
  }
}
