
import scala.io.Source

object Solution {
  case class Monkey(name: String, var value: Option[Long], var left: Option[Monkey], var right: Option[Monkey], var op: Option[String])

  def main(args: Array[String]): Unit = {
    val index = parse()
    index("humn").value = None
    index("root").op = Some("==")
    println(index("root").expect(0))
  }

  implicit class MonkeyOps(m: Monkey) {
    def solve(): Option[Long] = m.value match {
      case Some(v) => Some(v)
      case None =>
        if (m.left.isDefined && m.right.isDefined) {
          val left = m.left.get.solve()
          val right = m.right.get.solve()
          if (left.isDefined && right.isDefined) {
            m.op.get match {
              case "+" => Some(left.get + right.get)
              case "-" => Some(left.get - right.get)
              case "*" => Some(left.get * right.get)
              case "/" => Some(left.get / right.get)
              case "==" => if (left.get == right.get) Some(0) else Some(1)
            }
          } else None
        } else None
    }

    def expect(x: Long): Long = {
      if (m.name == "humn") return x
      val left = m.left.get.solve()
      val right = m.right.get.solve()
      if (left.isEmpty) {
        m.op.get match {
          case "+" => m.left.get.expect(x - right.get)
          case "-" => m.left.get.expect(x + right.get)
          case "*" => m.left.get.expect(x / right.get)
          case "/" => m.left.get.expect(x * right.get)
          case "==" => m.left.get.expect(right.get)
        }
      } else {
        m.op.get match {
          case "+" => m.right.get.expect(x - left.get)
          case "-" => m.right.get.expect(left.get - x)
          case "*" => m.right.get.expect(x / left.get)
          case "/" => m.right.get.expect(left.get / x)
          case "==" => m.right.get.expect(left.get)
        }
      }
    }
  }

  def parse(): Map[String, Monkey] = {
    val index = scala.collection.mutable.Map[String, Monkey]()
    def initMonkey(s: String): Unit = index.getOrElseUpdate(s, Monkey(s, None, None, None, None))
    Source.fromFile("input.txt").getLines().foreach { line =>
      val ff = line.split(": ")
      val goal = ff(0)
      initMonkey(goal)
      val num = try { Some(ff(1).toLong) } catch { case _: Throwable => None }
      if (num.isDefined) {
        index(goal).value = num
      } else {
        val r = ff(1).split(" ")
        val left = r(0)
        val op = r(1)
        val right = r(2)
        initMonkey(left)
        initMonkey(right)
        index(goal).left = Some(index(left))
        index(goal).op = Some(op)
        index(goal).right = Some(index(right))
      }
    }
    index.toMap
  }
}
