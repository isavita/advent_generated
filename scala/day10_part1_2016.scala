
import scala.io.Source

case class Bot(lowTo: String, highTo: String, chips: List[Int])

object Main extends App {
  val bots = collection.mutable.Map[String, Bot]()
  val valueRegex = "value (\\d+) goes to (bot \\d+)".r
  val givesRegex = "(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)".r

  for (line <- Source.fromFile("input.txt").getLines) {
    line match {
      case valueRegex(value, botID) =>
        bots.getOrElseUpdate(botID, Bot("", "", List())) match {
          case bot => bots.update(botID, bot.copy(chips = bot.chips :+ value.toInt))
        }
        
      case givesRegex(botID, lowTo, highTo) =>
        bots.getOrElseUpdate(botID, Bot("", "", List())) match {
          case bot => bots.update(botID, bot.copy(lowTo = lowTo, highTo = highTo))
        }
        
      case _ =>
    }
  }

  def minMax(a: Int, b: Int): (Int, Int) = {
    if (a < b) (a, b) else (b, a)
  }

  while (true) {
    var action = false
    for ((botID, b) <- bots) {
      if (b.chips.length == 2) {
        action = true
        val (low, high) = minMax(b.chips.head, b.chips(1))
        if (low == 17 && high == 61) {
          println(botID)
          System.exit(0)
        }
        bots(botID) = b.copy(chips = List())
        giveChip(bots, b.lowTo, low)
        giveChip(bots, b.highTo, high)
      }
    }
    if (!action) {
      System.exit(0)
    }
  }

  def giveChip(bots: collection.mutable.Map[String, Bot], target: String, value: Int): Unit = {
    val bot = bots.getOrElseUpdate(target, Bot("", "", List()))
    bots.update(target, bot.copy(chips = bot.chips :+ value))
  }
}
