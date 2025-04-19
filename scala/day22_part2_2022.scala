
import scala.io.Source
import scala.util.control.Breaks._

case class P(x: Int, y: Int)
case class Mov(steps: Int = 0, rot: Option[Char] = None)

object Main extends App {
  val lines = Source.fromFile("input.txt").getLines().toList
  val blank = lines.indexWhere(_.isEmpty)
  val size = lines.take(blank).head.length / 3
  val mapData = scala.collection.mutable.Map[P, Boolean]()
  lines.take(blank).zipWithIndex.foreach { case (l, r) =>
    l.zipWithIndex.foreach {
      case ('#', c) => mapData(P(r, c)) = true
      case ('.', c) => mapData(P(r, c)) = false
      case _        =>
    }
  }
  def parsePath(s: String) = {
    val buf = collection.mutable.ArrayBuffer[Mov]()
    var acc = 0
    s.foreach {
      case c @ ('R' | 'L') =>
        if (acc > 0) { buf += Mov(acc); acc = 0 }
        buf += Mov(0, Some(c))
      case d if d.isDigit  => acc = acc * 10 + d.asDigit
      case _               =>
    }
    if (acc > 0) buf += Mov(acc)
    buf.toList
  }
  val movements = parsePath(lines(blank + 1).trim)
  val dirs = Array(P(-1, 0), P(0, 1), P(1, 0), P(0, -1))
  def cross(n: P, dir: Int): (P, Int) = {
    val x = n.x; val y = n.y; val S = size
    if (x == -1 && y < 2 * S)        (P(y + 2 * S, x + 1), 1)
    else if (x == -1 && y >= 2 * S)  (P(x + 4 * S, y - 2 * S), 0)
    else if (x == S && dir == 2)     (P(y - S, x + S - 1), 3)
    else if (x == 2 * S - 1 && dir == 0) (P(y + S, x - S + 1), 1)
    else if (x == 3 * S && dir == 2) (P(y + 2 * S, x - 2 * S - 1), 3)
    else if (x == 4 * S)             (P(x - 4 * S, y + 2 * S), 2)
    else if (y == -1 && x < 3 * S)   (P(3 * S - 1 - x, y + S + 1), 1)
    else if (y == -1 && x >= 3 * S)  (P(y + 1, x - 2 * S), 2)
    else if (y == S - 1 && x < S)    (P(3 * S - 1 - x, y - S + 1), 1)
    else if (y == S - 1 && x >= S && dir == 3) (P(y + S + 1, x - S), 2)
    else if (y == S && dir == 1)     (P(y + 2 * S - 1, x - 2 * S), 0)
    else if (y == 2 * S && x < 2 * S && dir == 1) (P(y - S - 1, x + S), 0)
    else if (y == 2 * S && x >= 2 * S) (P(3 * S - 1 - x, y + S - 1), 3)
    else if (y == 3 * S)             (P(3 * S - 1 - x, y - S - 1), 3)
    else throw new RuntimeException("Invalid border")
  }

  var pos = P(0, size)
  var fac = 1

  movements.foreach { m =>
    m.rot.foreach(c => fac = (fac + (if (c == 'R') 1 else 3)) % 4)
    breakable {
      for (_ <- 0 until m.steps) {
        val d = dirs(fac)
        val np = P(pos.x + d.x, pos.y + d.y)
        val (npos, nfac) =
          if (mapData.contains(np))
            if (mapData(np)) (pos, fac) else (np, fac)
          else {
            val (cp, cf) = cross(np, fac)
            if (mapData.getOrElse(cp, true)) (pos, fac) else (cp, cf)
          }
        if (npos == pos && nfac == fac) break
        pos = npos; fac = nfac
      }
    }
  }

  val res = 1000 * (pos.x + 1) + 4 * (pos.y + 1) + ((fac + 3) % 4)
  println(res)
}
