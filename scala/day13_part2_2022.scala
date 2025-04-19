import scala.io.Source

sealed trait P
case class V(v: Int) extends P
case class L(l: List[P]) extends P

object Main extends App {
  val data = Source.fromFile("input.txt").mkString.trim
  def parse(s: String): P = {
    var i = 0
    def p(): P = if (s(i) == '[') {
      i += 1
      val buf = List.newBuilder[P]
      if (s(i) != ']') {
        buf += p()
        while (s(i) == ',') { i += 1; buf += p() }
      }
      i += 1
      L(buf.result())
    } else {
      var n = 0
      while (i < s.length && s(i).isDigit) { n = n * 10 + (s(i) - '0'); i += 1 }
      V(n)
    }
    p()
  }

  def cmp(a: P, b: P): Int = (a, b) match {
    case (V(x), V(y))       => x compare y
    case (V(_), L(_))       => cmp(L(List(a)), b)
    case (L(_), V(_))       => cmp(a, L(List(b)))
    case (L(xs), L(ys))     =>
      val it = xs.iterator.zipAll(ys.iterator, null, null)
      for ((u, v) <- it) {
        if (u == null) return -1
        if (v == null) return 1
        val c = cmp(u, v)
        if (c != 0) return c
      }
      0
  }

  val packets = data.split("\n\n").flatMap { block =>
    val Array(a, b) = block.split("\n")
    Array(parse(a), parse(b))
  }.toBuffer

  val d1 = L(List(L(List(V(2)))))
  val d2 = L(List(L(List(V(6)))))
  packets += d1
  packets += d2

  val sorted = packets.sortWith(cmp(_, _) < 0)
  val i1 = sorted.indexOf(d1) + 1
  val i2 = sorted.indexOf(d2) + 1
  println(i1 * i2)
}  