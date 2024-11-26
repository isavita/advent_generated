
import scala.io.Source

object TicketTranslation {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val sections = input.split("\n\n")

    val rules = parseRules(sections(0))
    val nearbyTickets = parseTickets(sections(2))
    val myTicket = parseTickets(sections(1))(0)

    // Part 1: Calculate ticket scanning error rate
    val errorRate = nearbyTickets
      .flatMap(ticket => ticket.filterNot(isValidForAnyRule(_, rules)))
      .sum
    println(s"Part 1 - Ticket Scanning Error Rate: $errorRate")

    // Part 2: Determine field order and calculate departure product
    val validTickets = nearbyTickets.filter(ticket => 
      ticket.forall(value => isValidForAnyRule(value, rules))
    )
    val fieldOrder = determineFieldOrder(rules, validTickets)
    val departureProduct = fieldOrder.zipWithIndex
      .filter { case (field, _) => field.startsWith("departure") }
      .map { case (_, index) => myTicket(index).toLong }
      .product
    println(s"Part 2 - Departure Product: $departureProduct")
  }

  def parseRules(rulesSection: String): Map[String, List[(Int, Int)]] = {
    rulesSection.split("\n").map { line =>
      val Array(field, ranges) = line.split(": ")
      val validRanges = ranges.split(" or ").map { range =>
        val Array(start, end) = range.split("-").map(_.toInt)
        (start, end)
      }.toList
      field -> validRanges
    }.toMap
  }

  def parseTickets(ticketSection: String): List[List[Int]] = {
    ticketSection.split("\n").drop(1).map(_.split(",").map(_.toInt).toList).toList
  }

  def isValidForAnyRule(value: Int, rules: Map[String, List[(Int, Int)]]): Boolean = {
    rules.values.exists(ranges => 
      ranges.exists { case (start, end) => value >= start && value <= end }
    )
  }

  def determineFieldOrder(rules: Map[String, List[(Int, Int)]], tickets: List[List[Int]]): List[String] = {
    val possibleFields = (0 until tickets.head.length).map { index =>
      val values = tickets.map(_(index))
      val validFields = rules.filter { case (_, ranges) =>
        values.forall(value => 
          ranges.exists { case (start, end) => value >= start && value <= end }
        )
      }.keys.toList
      index -> validFields
    }.toMap

    val fieldOrder = Array.fill(possibleFields.size)("")
    val usedFields = scala.collection.mutable.Set[String]()

    while (usedFields.size < possibleFields.size) {
      possibleFields.foreach { case (index, fields) =>
        val remainingFields = fields.filterNot(usedFields.contains)
        if (remainingFields.size == 1) {
          fieldOrder(index) = remainingFields.head
          usedFields.add(remainingFields.head)
        }
      }
    }

    fieldOrder.toList
  }
}
