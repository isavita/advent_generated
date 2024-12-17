
import scala.io.Source
import scala.util.Using
import scala.collection.mutable

object Main {

  case class Valve(id: String, flow: Int, tunnels: mutable.Map[String, Int])

  def main(args: Array[String]): Unit = {
    val valves = mutable.Map[String, Valve]()

    Using(Source.fromFile("input.txt")) { source =>
      source.getLines().foreach { line =>
        val parts = line.split("; ")
        val valveMatch = """Valve (\w+) has flow rate=(\d+)""".r.findFirstMatchIn(parts(0)).get
        val id = valveMatch.group(1)
        val flow = valveMatch.group(2).toInt
        val tunnelsPart = parts(1).substring(parts(1).indexOf("valve") + 6).trim
        val tunnels = mutable.Map(id -> 0)
        tunnelsPart.split(", ").foreach(t => tunnels(t) = 1)
        valves(id) = Valve(id, flow, tunnels)
      }
    }

    for {
      k <- valves.keys
      i <- valves.keys
      j <- valves.keys
      dik <- valves(i).tunnels.get(k)
      dkj <- valves(k).tunnels.get(j)
    } {
      val dij = valves(i).tunnels.get(j)
      if (dij.isEmpty || dij.get > dik + dkj) {
        valves(i).tunnels(j) = dik + dkj
      }
    }

    val openValves = valves.values.filter(_.flow > 0).map(_.id).toIndexedSeq
    var maxPressure = 0

    for (division <- divide(openValves.size)) {
      val (myValves, elephantValves) = (division(0), division(1))
      if (myValves.nonEmpty && elephantValves.nonEmpty) {
        val myOpen = myValves.map(openValves)
        val elephantOpen = elephantValves.map(openValves)
        val currentPressure = maxPressureCalc(valves, "AA", 26, 0, myOpen, 0) + maxPressureCalc(valves, "AA", 26, 0, elephantOpen, 0)
        maxPressure = maxPressure max currentPressure
      }
    }
    println(maxPressure)
  }

  def maxPressureCalc(valves: mutable.Map[String, Valve], current: String, minute: Int, pressure: Int, open: IndexedSeq[String], depth: Int): Int = {
    var max = pressure
    for (next <- open) {
      val newOpen = open.filterNot(_ == next)
      val timeLeft = minute - valves(current).tunnels(next) - 1
      if (timeLeft > 0) {
        max = max max maxPressureCalc(valves, next, timeLeft, timeLeft * valves(next).flow + pressure, newOpen, depth + 1)
      }
    }
    max
  }

  def divide(l: Int): IndexedSeq[IndexedSeq[IndexedSeq[Int]]] = {
    if (l == 1) {
      IndexedSeq(
        IndexedSeq(IndexedSeq(), IndexedSeq(0)),
        IndexedSeq(IndexedSeq(0), IndexedSeq())
      )
    } else {
      val prevDivisions = divide(l - 1)
      prevDivisions.flatMap { division =>
        IndexedSeq(
          IndexedSeq(division(0) :+ (l - 1), division(1)),
          IndexedSeq(division(0), division(1) :+ (l - 1))
        )
      }
    }
  }
}
