
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable

case class Bot(lowTo: String = "", highTo: String = "", chips: List[Int] = List())

object Main extends App {
  val bots = mutable.Map[String, Bot]()
  val outputs = mutable.Map[String, Int]()
  val valueRegex: Regex = """value (\d+) goes to (bot \d+)""".r
  val givesRegex: Regex = """(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)""".r

  for (line <- Source.fromFile("input.txt").getLines()) {
    line match {
      case valueRegex(value, botID) =>
        bots(botID) = bots.getOrElse(botID, Bot()).copy(chips = value.toInt :: bots.getOrElse(botID, Bot()).chips)
      case givesRegex(botID, lowTo, highTo) =>
        bots(botID) = bots.getOrElse(botID, Bot()).copy(lowTo = lowTo, highTo = highTo)
    }
  }

  def processBots(): Unit = {
    var action = true
    while (action) {
      action = false
      for ((botID, bot) <- bots if bot.chips.size == 2) {
        action = true
        val (low, high) = (bot.chips.min, bot.chips.max)
        bots(botID) = bot.copy(chips = List())
        giveChip(low, bot.lowTo)
        giveChip(high, bot.highTo)
      }
    }
  }

  def giveChip(value: Int, target: String): Unit = {
    if (target.startsWith("bot")) {
      bots(target) = bots.getOrElse(target, Bot()).copy(chips = value :: bots.getOrElse(target, Bot()).chips)
    } else if (target.startsWith("output")) {
      outputs(target) = value
    }
  }

  processBots()
  val result = outputs.getOrElse("output 0", 0) * outputs.getOrElse("output 1", 0) * outputs.getOrElse("output 2", 0)
  println(result)
}
