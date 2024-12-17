
import scala.io.Source
import scala.collection.mutable

object Main {

  sealed trait PulseValue
  case object Low extends PulseValue
  case object High extends PulseValue

  sealed trait ModuleType
  case object FlipFlop extends ModuleType
  case object Conjunction extends ModuleType
  case object Broadcaster extends ModuleType

  case class Module(name: String, moduleType: ModuleType, destinations: List[String], var state: Boolean = false, memory: mutable.Map[String, PulseValue] = mutable.Map.empty)

  case class Pulse(value: PulseValue, fromName: String, toName: String)

  def parseInput(input: List[String]): Map[String, Module] = {
    val modules = mutable.Map[String, Module]()

    input.foreach { line =>
      val parts = line.split(" -> ")
      val namePart = parts(0)
      val destinations = parts(1).split(", ").toList

      val (moduleType, name) = namePart.head match {
        case '%' => (FlipFlop, namePart.tail)
        case '&' => (Conjunction, namePart.tail)
        case _ => (Broadcaster, namePart)
      }
      modules(name) = Module(name, moduleType, destinations)
    }

    modules.values.foreach { module =>
      module.destinations.foreach { destName =>
        modules.get(destName) match {
          case Some(destModule) if destModule.moduleType == Conjunction =>
            destModule.memory(module.name) = Low
          case _ =>
        }
      }
    }
    modules.toMap
  }

  def pushButton(modules: Map[String, Module], startPulse: Pulse, numCycle: Int): (Long, Long) = {
    var cntLow = 0L
    var cntHigh = 0L
    val pulseQueue = mutable.Queue[Pulse]()

    for (_ <- 0 until numCycle) {
      pulseQueue.enqueue(startPulse)

      while (pulseQueue.nonEmpty) {
        val pulse = pulseQueue.dequeue()

        if (pulse.value == Low) {
          cntLow += 1
        } else {
          cntHigh += 1
        }

        modules.get(pulse.toName) match {
          case Some(module) =>
            val newPulses = module.moduleType match {
              case FlipFlop =>
                if (pulse.value == Low) {
                  module.state = !module.state
                  val newPulseValue = if (module.state) High else Low
                  module.destinations.map(destName => Pulse(newPulseValue, module.name, destName))
                } else {
                  List.empty
                }
              case Conjunction =>
                module.memory(pulse.fromName) = pulse.value
                val newPulseValue = if (module.memory.values.forall(_ == High)) Low else High
                module.destinations.map(destName => Pulse(newPulseValue, module.name, destName))
              case Broadcaster =>
                module.destinations.map(destName => Pulse(pulse.value, module.name, destName))
            }
            pulseQueue.enqueueAll(newPulses)
          case None =>
        }
      }
    }
    (cntLow, cntHigh)
  }

  def solve(input: List[String]): Long = {
    val startPulse = Pulse(Low, "button", "broadcaster")
    val numCycle = 1000

    val modules = parseInput(input)
    val (cntLow, cntHigh) = pushButton(modules, startPulse, numCycle)
    cntLow * cntHigh
  }

  def readFile(fileName: String): List[String] = {
    Source.fromFile(fileName).getLines().toList
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
