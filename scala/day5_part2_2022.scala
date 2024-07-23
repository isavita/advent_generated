
import scala.io.Source

object Main extends App {
  val Array(input, steps) = Source.fromFile("input.txt").mkString.split("\n\n").map(_.split("\n"))
  val stacks = Array.fill((input(0).length + 1) / 4)(List.empty[Char])

  input.foreach { line =>
    line.zipWithIndex.foreach { case (b, i) =>
      if (b.isUpper) stacks((i - 1) / 4) = b :: stacks((i - 1) / 4)
    }
  }

  steps.foreach { step =>
    val Array(_, nStr, _, fromStr, _, toStr) = step.split(" ")
    val n = nStr.toInt
    val from = fromStr.toInt - 1
    val to = toStr.toInt - 1
    val moved = stacks(from).takeRight(n)
    stacks(from) = stacks(from).dropRight(n)
    stacks(to) = stacks(to) ++ moved
  }

  println(stacks.map(_.last).mkString)
}
