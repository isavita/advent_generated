
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList.head.split(",").toList

  def spin(s: String, x: Int): String = s.takeRight(x) + s.dropRight(x)
  def exchange(s: String, a: Int, b: Int): String = s.updated(a, s(b)).updated(b, s(a))
  def partner(s: String, a: Char, b: Char): String = s.map(c => if (c == a) b else if (c == b) a else c)

  var programs = "abcdefghijklmnop"

  input.foreach {
    case s"s$size" => programs = spin(programs, size.toInt)
    case s"x$pos1/$pos2" => programs = exchange(programs, pos1.toInt, pos2.toInt)
    case s"p$prog1/$prog2" => programs = partner(programs, prog1.head, prog2.head)
  }

  println(programs)
}
