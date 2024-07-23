
import scala.io.Source
import scala.collection.mutable

case class Valve(id: String, flow: Int, tunnels: mutable.Map[String, Int])

object Main {
  def main(args: Array[String]): Unit = {
    val valves = mutable.Map[String, Valve]()
    val input = Source.fromFile("input.txt").getLines().mkString("\n")

    input.split("\n").foreach { line =>
      val Array(vInfo, tInfo) = line.split("; ")
      val Array(id, flowRate) = vInfo.split(" has flow rate=")
      val flow = flowRate.toInt
      val idVal = id.split(" ")(1)
      val tunnels = mutable.Map[String, Int](idVal -> 0)

      val tunnelList = tInfo.substring(tInfo.indexOf("to valve") + 9).split(", ").map(_.trim)
      tunnelList.foreach(t => tunnels(t) = 1)

      valves(idVal) = Valve(idVal, flow, tunnels)
    }

    for (k <- valves.keys) {
      for (i <- valves.keys) {
        for (j <- valves.keys) {
          val dik = valves(i).tunnels.get(k)
          val dkj = valves(k).tunnels.get(j)
          if (dik.isDefined && dkj.isDefined) {
            val dij = valves(i).tunnels.get(j)
            if (dij.isEmpty || dij.get > dik.get + dkj.get) {
              valves(i).tunnels(j) = dik.get + dkj.get
            }
          }
        }
      }
    }

    val open = valves.values.filter(_.flow > 0).map(_.id).toList
    println(maxPressure(valves, "AA", 30, 0, open))
  }

  def maxPressure(valves: mutable.Map[String, Valve], curr: String, minute: Int, pressure: Int, open: List[String]): Int = {
    open.foldLeft(pressure) { (max, next) =>
      val timeLeft = minute - valves(curr).tunnels(next) - 1
      if (timeLeft > 0) {
        val newOpen = open.filterNot(_ == next)
        val newPressure = timeLeft * valves(next).flow + pressure
        max.max(maxPressure(valves, next, timeLeft, newPressure, newOpen))
      } else max
    }
  }
}
