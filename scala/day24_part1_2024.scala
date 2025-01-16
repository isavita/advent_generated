
import scala.io.Source
import scala.util.matching.Regex

object LogicGates {

  case class Gate(input1: String, input2: String, operation: String, output: String)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val wireRegex = new Regex("""^(\w+):\s*([01])$""")
    val gateRegex = new Regex("""^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$""")
    val (wires, gates) = parseInput(lines, wireRegex, gateRegex)
    val zWires = simulateGates(wires, gates)
    val decimalValue = binaryToDecimal(zWires)
    println(decimalValue)
  }

  def parseInput(lines: List[String], wireRegex: Regex, gateRegex: Regex): (Map[String, Int], List[Gate]) = {
    val wires = scala.collection.mutable.Map[String, Int]()
    val gates = scala.collection.mutable.ListBuffer[Gate]()
    var parsingWires = true

    for (line <- lines) {
      if (line.trim.isEmpty) {
        parsingWires = false
      } else if (parsingWires) {
        wireRegex.findFirstMatchIn(line.trim) match {
          case Some(m) => wires += (m.group(1) -> m.group(2).toInt)
          case None => throw new RuntimeException(s"Invalid wire definition: $line")
        }
      } else {
        gateRegex.findFirstMatchIn(line.trim) match {
          case Some(m) => gates += Gate(m.group(1), m.group(3), m.group(2), m.group(4))
          case None => throw new RuntimeException(s"Invalid gate definition: $line")
        }
      }
    }
    (wires.toMap, gates.toList)
  }

  def simulateGates(initialWires: Map[String, Int], gates: List[Gate]): Map[Int, Int] = {
    val wires = scala.collection.mutable.Map(initialWires.toSeq: _*)
    var remainingGates = gates
    while (remainingGates.nonEmpty) {
      var progress = false
      val newRemainingGates = scala.collection.mutable.ListBuffer[Gate]()
      for (gate <- remainingGates) {
        (wires.get(gate.input1), wires.get(gate.input2)) match {
          case (Some(val1), Some(val2)) =>
            val outputVal = gate.operation match {
              case "AND" => if (val1 == 1 && val2 == 1) 1 else 0
              case "OR" => if (val1 == 1 || val2 == 1) 1 else 0
              case "XOR" => if (val1 != val2) 1 else 0
            }
            wires += (gate.output -> outputVal)
            progress = true
          case _ => newRemainingGates += gate
        }
      }
      if (!progress) {
        throw new RuntimeException("Cannot evaluate remaining gates due to missing inputs or cyclic dependencies.")
      }
      remainingGates = newRemainingGates.toList
    }
    val zRegex = new Regex("""^z(\d+)$""")
    wires.collect { case (wire, value) if zRegex.findFirstMatchIn(wire).isDefined =>
      val m = zRegex.findFirstMatchIn(wire).get
      m.group(1).toInt -> value
    }.toMap
  }

  def binaryToDecimal(zWires: Map[Int, Int]): Long = {
    if (zWires.isEmpty) {
      throw new RuntimeException("No wires starting with 'z' found.")
    }
    val sortedIndices = zWires.keys.toSeq.sorted
    val binaryString = sortedIndices.reverse.map(zWires).mkString
    java.lang.Long.parseLong(binaryString, 2)
  }
}
