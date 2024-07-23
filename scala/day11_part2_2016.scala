
import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object RtgHellDay {
  def main(args: Array[String]): Unit = {
    val ans = rtgHellDay(readFile("input.txt"))
    println(ans)
  }

  def rtgHellDay(input: String): Int = {
    val initialState = newInitialState(input)
    val queue = mutable.Queue(initialState)
    val prevStates = mutable.Set[String]()

    while (queue.nonEmpty) {
      val front = queue.dequeue()

      if (front.isDone) return front.steps

      val hash = front.hashKey()
      if (prevStates.contains(hash)) continue

      prevStates.add(hash)
      queue ++= front.getNextStates()
    }

    -1
  }

  case class Halves(isChip: Boolean, material: String)

  case class State(floors: Array[ArrayBuffer[Halves]], elevatorLevel: Int, steps: Int) {
    def isValid: Boolean = {
      floors.zipWithIndex.forall { case (floor, _) =>
        val gensSeen = mutable.Set[String]()
        floor.foreach {
          case Halves(false, material) => gensSeen.add(material)
          case Halves(true, material) if !gensSeen.contains(material) => return false
          case _ =>
        }
        true
      }
    }

    def isDone: Boolean = floors.take(3).forall(_.isEmpty)

    def hashKey(): String = {
      val (genMap, chipMap) = floors.zipWithIndex.flatMap { case (floor, index) =>
        floor.map {
          case Halves(false, material) => material -> index
          case Halves(true, material) => material -> index
        }
      }.partition { case (_, isChip) => isChip }

      val genChipPairs = genMap.keys.toSeq.map { material =>
        (genMap(material), chipMap(material))
      }.sortBy { case (g, c) => (g, c) }

      s"$elevatorLevel ${genChipPairs.mkString(",")}"
    }

    def getMovablePermIndices: Seq[Seq[Int]] = {
      val currentLevel = floors(elevatorLevel)
      val permsToMove = ArrayBuffer[Seq[Int]]()

      for (i <- currentLevel.indices) {
        permsToMove += Seq(i)
        for (j <- i + 1 until currentLevel.length) {
          permsToMove += Seq(i, j)
        }
      }
      permsToMove
    }

    def clone(): State = {
      State(floors.map(_.clone()), elevatorLevel, steps)
    }

    def getNextStates: Seq[State] = {
      val futureStates = ArrayBuffer[State]()
      val movablePermIndices = getMovablePermIndices
      val eleDiffs = Seq(-1, 1).filter(d => elevatorLevel + d >= 0 && elevatorLevel + d < floors.length)

      for (eleDiff <- eleDiffs; permIndices <- movablePermIndices) {
        val cl = clone()
        cl.elevatorLevel += eleDiff
        cl.steps += 1

        val oldLevel = elevatorLevel
        val newLevel = cl.elevatorLevel

        permIndices.foreach { index =>
          cl.floors(newLevel) += cl.floors(oldLevel)(index)
        }

        permIndices.reverse.foreach { index =>
          cl.floors(oldLevel).remove(index)
        }

        if (cl.isValid) futureStates += cl
      }

      futureStates
    }
  }

  def newInitialState(input: String): State = {
    val floors = Array.fill(4)(ArrayBuffer[Halves]())
    input.split("\n").zipWithIndex.foreach { case (line, lineIndex) =>
      val parts = line.split("\\s+").map(_.replaceAll("[,.]", ""))
      parts.sliding(2).foreach {
        case Array(material, "generator") => floors(lineIndex) += Halves(false, material)
        case Array(material, "microchip") => floors(lineIndex) += Halves(true, material.stripSuffix("-comp"))
      }
    }
    State(floors, 0, 0)
  }

  def readFile(path: String): String = {
    Source.fromFile(path).getLines().mkString("\n").trim
  }
}
