
import scala.io.Source
import scala.util.Using

object Main {

  case class Monkey(
      var items: Array[Long],
      operation: Long => Long,
      div: Int,
      next: Array[Int],
      var inspections: Long = 0
  )

  def parse(s: String): Monkey = {
    val lines = s.split("\n")
    val items = lines(1).split(": ")(1).split(", ").map(_.toLong).toArray
    val f = lines(2).split("= ")(1).split(" ")
    val operation: Long => Long = f(1) match {
      case "+" =>
        f(2) match {
          case "old" => (old: Long) => old + old
          case _     => (old: Long) => old + f(2).toLong
        }
      case "*" =>
        f(2) match {
          case "old" => (old: Long) => old * old
          case _     => (old: Long) => old * f(2).toLong
        }
    }
    val div = lines(3).split(" ").last.toInt
    val next = Array(
      lines(4).split(" ").last.toInt,
      lines(5).split(" ").last.toInt
    )
    Monkey(items, operation, div, next)
  }

  def monkeyBusiness(monkeys: Array[Monkey], rounds: Int, worry: Boolean): Long = {
    val div = monkeys.map(_.div.toLong).product
    for (_ <- 0 until rounds) {
      for (m <- monkeys) {
        while (m.items.nonEmpty) {
          m.inspections += 1
          var item = m.operation(m.items(0))
          if (worry) {
            item %= div
          } else {
            item /= 3
          }
          val nextMonkey = if (item % m.div == 0) m.next(0) else m.next(1)
          monkeys(nextMonkey).items = monkeys(nextMonkey).items :+ item
          m.items = m.items.tail
        }
      }
    }
    monkeys.map(_.inspections).sorted.reverse.take(2).product
  }

  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromFile("input.txt"))(_.mkString).get
    val monkeys = input.split("\n\n").map(parse).toArray
    println(monkeyBusiness(monkeys.clone(), 10000, true))
  }
}
