
import scala.io.Source

object Solution extends App {
  val lines = Source.fromFile("input.txt").getLines().toList

  val l = (0 until 14).map(i => lines(i * 18 + 4).split(" ").last.toInt)
  val k = (0 until 14).map(i => lines(i * 18 + 5).split(" ").last.toInt)
  val m = (0 until 14).map(i => lines(i * 18 + 15).split(" ").last.toInt)

  val constraints = scala.collection.mutable.Map[Int, (Int, Int)]()
  val stack = scala.collection.mutable.Stack[Int]()

  for (i <- l.indices) {
    l(i) match {
      case 1 => stack.push(i)
      case 26 =>
        val pop = stack.pop()
        constraints(pop) = (i, m(pop) + k(i))
    }
  }

  val max = Array.fill(14)(0)
  for (i <- constraints.keys) {
    var vmax = 9
    while (vmax + constraints(i)._2 > 9) vmax -= 1
    max(i) = vmax
    max(constraints(i)._1) = vmax + constraints(i)._2
  }

  println(max.map(x => x.toString).mkString.toLong)
}
