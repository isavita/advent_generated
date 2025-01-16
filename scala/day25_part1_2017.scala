
import scala.io.Source

object TuringMachine {

  case class Rule(write: Int, move: Int, nextState: Char)
  case class State(rules: Map[Int, Rule])

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList

    val initialState = lines.head.split(" ").last.head
    val steps = lines(1).split(" ")(5).toInt

    val states = parseStates(lines.drop(3))

    val tape = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    var cursor = 0
    var currentState = initialState

    for (_ <- 0 until steps) {
      val currentValue = tape(cursor)
      val rule = states(currentState).rules(currentValue)
      tape(cursor) = rule.write
      cursor += rule.move
      currentState = rule.nextState
    }

    println(tape.values.count(_ == 1))
  }

  def parseStates(lines: List[String]): Map[Char, State] = {
    lines.grouped(10).map { stateLines =>
      val stateName = stateLines.head.split(" ").last.head
      val rule0 = Rule(
        write = stateLines(2).split(" ").last.head.asDigit,
        move = if (stateLines(3).contains("right")) 1 else -1,
        nextState = stateLines(4).split(" ").last.head
      )
      val rule1 = Rule(
        write = stateLines(6).split(" ").last.head.asDigit,
        move = if (stateLines(7).contains("right")) 1 else -1,
        nextState = stateLines(8).split(" ").last.head
      )
      stateName -> State(Map(0 -> rule0, 1 -> rule1))
    }.toMap
  }
}
