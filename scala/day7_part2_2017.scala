
import scala.io.Source

case class Program(weight: Int, holds: List[String])

object Solution {
  def dfs(name: String, programs: Map[String, Program]): (Int, Boolean) = {
    val program = programs(name)
    var totalWeight = program.weight
    var weights = Map[Int, Int]()

    for (child <- program.holds) {
      val (weight, balanced) = dfs(child, programs)
      if (!balanced) {
        return (0, false)
      }
      totalWeight += weight
      weights += weight -> (weights.getOrElse(weight, 0) + 1)
    }

    for ((w1, c1) <- weights) {
      for ((w2, c2) <- weights) {
        if (w1 != w2 && c1 < c2) {
          var unbalancedProgram = ""
          for (child <- program.holds) {
            if (dfs(child, programs)._1 == w1) {
              unbalancedProgram = child
            }
          }
          println(programs(unbalancedProgram).weight + (w2 - w1))
          return (0, false)
        }
      }
    }
    (totalWeight, true)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines.toList

    var programs = Map[String, Program]()

    val pattern = """[a-z]+|\d+""".r

    for (line <- lines) {
      val matches = pattern.findAllIn(line).toList
      val name = matches(0)
      val weight = matches(1).toInt
      val program = Program(weight, if (matches.length > 2) matches.slice(2, matches.length) else List())
      programs += name -> program
    }

    val root = "dtacyn" // Replace this with the root found in Part One
    dfs(root, programs)
  }
}
