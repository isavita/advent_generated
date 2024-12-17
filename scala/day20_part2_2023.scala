
import scala.io.Source
import scala.collection.mutable
import scala.util.Using

object Main {

  sealed trait Module {
    val name: String
    val connectsTo: List[String]
  }

  case class FlipFlop(name: String, connectsTo: List[String], var state: Boolean = false) extends Module
  case class Conjunction(name: String, connectsTo: List[String], watches: mutable.Map[String, Boolean] = mutable.Map.empty) extends Module
  case class Broadcaster(name: String, connectsTo: List[String]) extends Module

  case class State(from: String, name: String, pulse: Boolean)

  def handleLine(line: String, connections: mutable.Map[String, Module]): Unit = {
    if (line.contains("broadcaster")) {
      val Array(name, targets) = line.split(" -> ")
      connections(name) = Broadcaster(name, targets.split(", ").toList)
    } else if (line.contains("%")) {
      val Array(name, targets) = line.split(" -> ")
      connections(name.substring(1)) = FlipFlop(name.substring(1), targets.split(", ").toList)
    } else {
      val Array(name, targets) = line.split(" -> ")
      connections(name.substring(1)) = Conjunction(name.substring(1), targets.split(", ").toList)
    }
  }

  def completeWatches(connections: mutable.Map[String, Module]): Unit = {
    val conjunctions = connections.values.collect { case c: Conjunction => c }
    for (conj <- conjunctions) {
      for (module <- connections.values) {
        module match {
          case ff: FlipFlop => if (ff.connectsTo.contains(conj.name)) conj.watches(ff.name) = false
          case cj: Conjunction => if (cj.connectsTo.contains(conj.name)) conj.watches(cj.name) = false
          case _ =>
        }
      }
    }
  }

  def simulatePress(connections: mutable.Map[String, Module], loops: mutable.Map[String, Int], pressNumber: Int): (Array[Int], Boolean) = {
    val queue = mutable.Queue(State("button", "broadcaster", false))
    val pulses = Array(1, 0)
    var found = false

    while (queue.nonEmpty) {
      val currState = queue.dequeue()
      val module = connections.get(currState.name)

      if (currState.name == "out") {
        
      } else if (currState.name == "rx" && !currState.pulse) {
        found = true
      } else {
        module match {
          case Some(b: Broadcaster) =>
            b.connectsTo.foreach { name =>
              queue.enqueue(State(b.name, name, currState.pulse))
              pulses(if (currState.pulse) 1 else 0) += 1
            }
          case Some(ff: FlipFlop) =>
            if (!currState.pulse) {
              ff.state = !ff.state
              ff.connectsTo.foreach { name =>
                queue.enqueue(State(ff.name, name, ff.state))
                pulses(if (ff.state) 1 else 0) += 1
              }
            }
          case Some(cj: Conjunction) =>
            cj.watches(currState.from) = currState.pulse
            val allTrue = cj.watches.values.forall(identity)
            cj.connectsTo.foreach { name =>
              queue.enqueue(State(cj.name, name, !allTrue))
              pulses(if (!allTrue) 1 else 0) += 1
            }
            if (loops.contains(cj.name) && !allTrue && loops(cj.name) == -1) {
              loops(cj.name) = pressNumber
            }
          case _ =>
        }
      }
    }
    (pulses, found)
  }

  def connectsTo(from: String, to: String, connections: mutable.Map[String, Module]): Boolean = {
    connections.get(from) match {
      case Some(b: Broadcaster) => b.connectsTo.contains(to)
      case Some(ff: FlipFlop) => ff.connectsTo.contains(to)
      case Some(cj: Conjunction) => cj.connectsTo.contains(to)
      case _ => false
    }
  }

  def main(args: Array[String]): Unit = {
    val connections = mutable.Map[String, Module]()

    Using(Source.fromFile("input.txt")) { source =>
      source.getLines().foreach(line => handleLine(line, connections))
    }

    completeWatches(connections)

    val pxPrev = connections.keys.filter(k => connectsTo(k, "rx", connections)).toList
    if (pxPrev.length != 1) {
      throw new Exception("Error: more than one pxPrev")
    }

    val conj = connections(pxPrev.head) match {
      case c: Conjunction => c
      case _ => throw new Exception("Error: pxPrev is not a conjunction")
    }

    val loopLengths = mutable.Map[String, Int]()
    conj.watches.keys.foreach(name => loopLengths(name) = -1)

    var pressNumber = 0
    var found = false
    while (!found) {
      pressNumber += 1
      val (pulses, f) = simulatePress(connections, loopLengths, pressNumber)
      found = f
      if (!found && loopLengths.values.forall(_ != -1)) {
        found = true
      }
    }

    val sum = loopLengths.values.foldLeft(1L)(_ * _)
    println(sum)
  }
}
