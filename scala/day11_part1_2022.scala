import scala.io.Source
import scala.util.Try

case class Monkey(var items: List[Int], operation: Int => Int, div: Int, next: Array[Int], var inspections: Int = 0)

object MonkeyBusiness {
  def main(args: Array[String]): Unit = {
    val monkeys = Source.fromFile("input.txt").mkString.split("\n\n").map(parse).toList
    val div = monkeys.map(_.div).product
    for (_ <- 1 to 20) {
      for (monkey <- monkeys) {
        for (item <- monkey.items) {
          monkey.inspections += 1
          val newItem = monkey.operation(item) / 3
          val nextMonkey = if (newItem % monkey.div == 0) monkey.next(0) else monkey.next(1)
          monkeys(nextMonkey).items = newItem :: monkeys(nextMonkey).items
        }
        monkey.items = Nil
      }
    }
    val inspections = monkeys.map(_.inspections).sorted.reverse
    println(inspections(0) * inspections(1))
  }

  def parse(s: String): Monkey = {
    val lines = s.split("\n")
    val items = lines(1).split(": ").last.split(", ").map(_.trim.toInt).toList
    val operation = lines(2).split(": ").last match {
      case s"new = old * old" => (old: Int) => old * old
      case s"new = old * $n" => (old: Int) => old * n.toInt
      case s"new = old + old" => (old: Int) => old + old
      case s"new = old + $n" => (old: Int) => old + n.toInt
    }
    val div = lines(3).split(" ").last.toInt
    val next = Array(lines(4).split(" ").last.toInt, lines(5).split(" ").last.toInt)
    Monkey(items, operation, div, next)
  }
}