
import scala.io.Source

object Solution extends App {
  val lines = Source.fromFile("input.txt").getLines().toList

  val l = lines.zipWithIndex.filter(_._2 % 18 == 4).map(x => x._1.split(" ").last.toInt)
  val k = lines.zipWithIndex.filter(_._2 % 18 == 5).map(x => x._1.split(" ").last.toInt)
  val m = lines.zipWithIndex.filter(_._2 % 18 == 15).map(x => x._1.split(" ").last.toInt)

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

  val min = Array.fill(14)(0)
  for (i <- constraints.keys) {
    val vmin = 1 + math.max(0, -constraints(i)._2)
    min(i) = vmin
    min(constraints(i)._1) = vmin + constraints(i)._2
  }

  println(min.mkString("").toLong)
}
