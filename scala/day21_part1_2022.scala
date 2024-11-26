
import scala.io.Source
import scala.collection.mutable

object MonkeyMath {
  sealed trait Job
  case class Number(value: Long) extends Job
  case class Operation(left: String, op: String, right: String) extends Job

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val monkeys = parseMonkeys(input)
    val result = calculateRootValue(monkeys)
    println(result)
  }

  def parseMonkeys(lines: List[String]): mutable.Map[String, Job] = {
    val monkeys = mutable.Map[String, Job]()
    
    lines.foreach { line =>
      val parts = line.split(": ")
      val monkey = parts(0)
      val jobParts = parts(1).split(" ")
      
      val job = if (jobParts.length == 1) {
        Number(jobParts(0).toLong)
      } else {
        Operation(jobParts(0), jobParts(1), jobParts(2))
      }
      
      monkeys(monkey) = job
    }
    
    monkeys
  }

  def calculateRootValue(monkeys: mutable.Map[String, Job]): Long = {
    val cache = mutable.Map[String, Long]()

    def evaluate(monkey: String): Long = {
      if (cache.contains(monkey)) return cache(monkey)

      val result = monkeys(monkey) match {
        case Number(value) => value
        case Operation(left, op, right) =>
          val leftValue = evaluate(left)
          val rightValue = evaluate(right)
          op match {
            case "+" => leftValue + rightValue
            case "-" => leftValue - rightValue
            case "*" => leftValue * rightValue
            case "/" => leftValue / rightValue
          }
      }

      cache(monkey) = result
      result
    }

    evaluate("root")
  }
}
