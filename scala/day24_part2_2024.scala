
import scala.io.Source
import scala.collection.mutable

object Solution {
  case class Gate(a: String, op: String, b: String)

  def parse(input: String): List[(Gate, String)] = {
    val parts = input.split("\n\n")
    if (parts.length != 2) return List.empty

    parts(1).split("\n").filter(_.nonEmpty).flatMap { line =>
      val lineParts = line.split(" -> ")
      if (lineParts.length != 2) None
      else {
        val gateParts = lineParts(0).split(" ")
        if (gateParts.length != 3) None
        else Some((Gate(gateParts(0), gateParts(1), gateParts(2)), lineParts(1)))
      }
    }.toList
  }

  def createLookups(gates: List[(Gate, String)]): (mutable.Map[String, Gate], mutable.Map[String, String]) = {
    val lookup = mutable.Map.empty[String, Gate]
    val reverseLookup = mutable.Map.empty[String, String]

    gates.foreach { case (gate, output) =>
      lookup += (output -> gate)
      val inputs = List(gate.a, gate.b).sorted
      val key = s"${inputs(0)}_${gate.op}_${inputs(1)}"
      reverseLookup += (key -> output)
    }
    (lookup, reverseLookup)
  }

  def swap(pairs: mutable.ArrayBuffer[(String, String)], gates: mutable.ArrayBuffer[(Gate, String)], a: String, b: String): Unit = {
    pairs += ((a, b))
    gates.indices.foreach { i =>
      if (gates(i)._2 == a) gates(i) = (gates(i)._1, b)
      else if (gates(i)._2 == b) gates(i) = (gates(i)._1, a)
    }
  }

  def getReverseLookupKey(a: String, op: String, b: String): String = {
    val inputs = List(a, b).sorted
    s"${inputs(0)}_${op}_${inputs(1)}"
  }

  def solution(gatesList: List[(Gate, String)]): String = {
    val gates = mutable.ArrayBuffer.from(gatesList)
    val pairs = mutable.ArrayBuffer.empty[(String, String)]
    val numZ = gates.count { case (_, output) => output.startsWith("z") }

    while (pairs.length < 4) {
      var adder = ""
      var carry = ""
      val (lookup, reverseLookup) = createLookups(gates.toList)

      for (i <- 0 until numZ) {
        val xi = f"x${i}%02d"
        val yi = f"y${i}%02d"
        val zi = f"z${i}%02d"

        if (i == 0) {
          adder = reverseLookup.getOrElse(getReverseLookupKey(xi, "XOR", yi), "")
          carry = reverseLookup.getOrElse(getReverseLookupKey(xi, "AND", yi), "")
        } else {
          val bit = reverseLookup.getOrElse(getReverseLookupKey(xi, "XOR", yi), "")
          if (bit.nonEmpty) {
            adder = reverseLookup.getOrElse(getReverseLookupKey(bit, "XOR", carry), "")
            if (adder.nonEmpty) {
              val c1 = reverseLookup.getOrElse(getReverseLookupKey(xi, "AND", yi), "")
              val c2 = reverseLookup.getOrElse(getReverseLookupKey(bit, "AND", carry), "")
              carry = reverseLookup.getOrElse(getReverseLookupKey(c1, "OR", c2), "")
            }
          }
        }

        if (adder.isEmpty) {
          lookup.get(zi) match {
            case Some(gate) =>
              val bitKey = getReverseLookupKey(xi, "XOR", yi)
              val bit = reverseLookup.getOrElse(bitKey, "")
              if (reverseLookup.contains(getReverseLookupKey(gate.a, "XOR", carry))) {
                swap(pairs, gates, bit, gate.a)
                
              } else if (reverseLookup.contains(getReverseLookupKey(gate.b, "XOR", carry))) {
                swap(pairs, gates, bit, gate.b)
                
              }
            case None =>
          }
        } else if (adder != zi) {
          swap(pairs, gates, adder, zi)
          
        }
      }
    }

    pairs.flatMap(p => List(p._1, p._2)).sorted.mkString(",")
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val gates = parse(input)
    println(solution(gates))
  }
}
