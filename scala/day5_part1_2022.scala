
import scala.io.Source

object Solution {
  def move(stacks: Array[List[Char]], steps: Array[String]): String = {
    val mutableStacks = stacks.map(_.toBuffer)

    for (step <- steps) {
      val parts = step.split(" ")
      val n     = parts(1).toInt
      val from  = parts(3).toInt - 1
      val to    = parts(5).toInt - 1

      for (_ <- 0 until n) {
        mutableStacks(to) += mutableStacks(from).last
        mutableStacks(from).remove(mutableStacks(from).length - 1)
      }
    }

    mutableStacks.map(_.last).mkString
  }

  def main(args: Array[String]): Unit = {
    val data              = Source.fromFile("input.txt").mkString.split("\n\n")
    val input_            = data(0).split("\n")
    val stacks = Array.fill((input_(0).length + 1) / 4)(List[Char]())
    
    for (line <- input_) {
      for ((b, i) <- line.zipWithIndex) {
        if ('A' <= b && b <= 'Z') {
          stacks((i - 1) / 4) = b :: stacks((i-1)/4)
        }
      }
    }
      val steps = data(1).split("\n").filterNot(_.isEmpty)
      println(move(stacks,steps))
  }
}
