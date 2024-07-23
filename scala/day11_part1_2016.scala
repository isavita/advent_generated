
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
      if (front.isDone()) return front.steps

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
      floors.indices.forall { i =>
        val gensSeen = mutable.Set[String]()
        floors(i).forall { half =>
          if (!half.isChip) gensSeen.add(half.material)
          else !gensSeen.contains(half.material) || gensSeen.size == 0
        }
      }
    }

    def isDone: Boolean = floors.take(3).map(_.length).sum == 0

    def hashKey(): String = {
      val (genMap, chipMap) = floors.zipWithIndex.flatMap { case (fl, idx) =>
        fl.map { half =>
          if (half.isChip) (half.material, -1, idx) else (half.material, idx, -1)
        }
      }.partition(_._2 == -1)

      val genChipPairs = genMap.map { case (mat, _, idx) => (mat, idx) }
        .zip(chipMap.map { case (mat, idx, _) => (mat, idx) })
        .map { case ((mat, genIdx), (_, chipIdx)) => (genIdx, chipIdx) }

      s"$elevatorLevel:${genChipPairs.sorted.mkString(",")}"
    }

    def getMovablePermIndices: Seq[Seq[Int]] = {
      val currentLevel = floors(elevatorLevel)
      val permsToMove = ArrayBuffer[Seq[Int]]()

      for (i <- currentLevel.indices) {
        for (j <- i + 1 until currentLevel.length) {
          permsToMove.append(Seq(i, j))
        }
        permsToMove.append(Seq(i))
      }
      permsToMove
    }

    def clone(): State = State(floors.map(_.clone()), elevatorLevel, steps)

    def getNextStates: Seq[State] = {
      val futureStates = ArrayBuffer[State]()
      val movablePermIndices = getMovablePermIndices
      val eleDiffs = Seq(-1, 1).filter(d => elevatorLevel + d >= 0 && elevatorLevel + d < floors.length)

      for (eleDiff <- eleDiffs) {
        for (permIndices <- movablePermIndices) {
          val cl = clone()
          cl.elevatorLevel += eleDiff
          cl.steps += 1
          val oldLevel = elevatorLevel
          val newLevel = cl.elevatorLevel

          permIndices.foreach { index =>
            cl.floors(newLevel).append(cl.floors(oldLevel)(index))
          }

          permIndices.reverse.foreach { index =>
            cl.floors(oldLevel)(index) = cl.floors(oldLevel).last
            cl.floors(oldLevel).trimEnd(1)
          }

          if (cl.isValid) futureStates.append(cl)
        }
      }
      futureStates
    }
  }

  def newInitialState(input: String): State = {
    val floors = Array.fill(4)(ArrayBuffer[Halves]())
    input.split("\n").zipWithIndex.foreach { case (line, lineIndex) =>
      val parts = line.split(" ").map(_.replaceAll("[,\\.]", ""))
      parts.indices.foreach { i =>
        if (parts(i) == "generator") {
          val material = parts(i - 1)
          floors(lineIndex).append(Halves(false, material))
        } else if (parts(i) == "microchip") {
          val material = parts(i - 1).split("-")(0)
          floors(lineIndex).append(Halves(true, material))
        }
      }
    }
    State(floors, 0, 0)
  }

  def readFile(path: String): String = {
    Source.fromFile(path).getLines().mkString("\n").trim
  }
}
