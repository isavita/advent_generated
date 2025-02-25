
import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.PriorityQueue

object Main {

  class Machine(program: Array[Long], inputQueue: mutable.Queue[Long], outputQueue: mutable.Queue[Long]) {
    val data: mutable.Map[Long, Long] = mutable.Map.empty[Long, Long].withDefaultValue(0L)
    for ((value, index) <- program.zipWithIndex) {
      data(index.toLong) = value
    }
    var ip: Long = 0
    var relbase: Long = 0

    def get(i: Long, mo: Long): Long = {
      mo match {
        case 0 => data(data(i))
        case 1 => data(i)
        case 2 => data(relbase + data(i))
        case _ => throw new IllegalArgumentException(s"Unknown mode: $mo")
      }
    }

    def set(i: Long, mo: Long, value: Long): Unit = {
      mo match {
        case 0 => data(data(i)) = value
        case 2 => data(relbase + data(i)) = value
        case _ => throw new IllegalArgumentException(s"Unknown mode: $mo")
      }
    }

    def step(): Boolean = {
      val op = (data(ip) % 100).toInt
      val modes = (2 to 4).map(i => (data(ip) / math.pow(10, i).toLong) % 10)

      op match {
        case 1 =>
          set(ip + 3, modes(2), get(ip + 1, modes(0)) + get(ip + 2, modes(1)))
          ip += 4
          true
        case 2 =>
          set(ip + 3, modes(2), get(ip + 1, modes(0)) * get(ip + 2, modes(1)))
          ip += 4
          true
        case 3 =>
          if (inputQueue.isEmpty) return false
          set(ip + 1, modes(0), inputQueue.dequeue())
          ip += 2
          true
        case 4 =>
          outputQueue.enqueue(get(ip + 1, modes(0)))
          ip += 2
          true
        case 5 =>
          ip = if (get(ip + 1, modes(0)) != 0) get(ip + 2, modes(1)) else ip + 3
          true
        case 6 =>
          ip = if (get(ip + 1, modes(0)) == 0) get(ip + 2, modes(1)) else ip + 3
          true
        case 7 =>
          set(ip + 3, modes(2), if (get(ip + 1, modes(0)) < get(ip + 2, modes(1))) 1 else 0)
          ip += 4
          true
        case 8 =>
          set(ip + 3, modes(2), if (get(ip + 1, modes(0)) == get(ip + 2, modes(1))) 1 else 0)
          ip += 4
          true
        case 9 =>
          relbase += get(ip + 1, modes(0))
          ip += 2
          true
        case 99 =>
          false
        case _ => throw new IllegalArgumentException(s"Unknown opcode: $op")
      }
    }

    def run(): Unit = {
      while (step()) {}
    }
  }

  def manhattan(p: (Int, Int), q: (Int, Int)): Int = {
    math.abs(p._1 - q._1) + math.abs(p._2 - q._2)
  }

  class Pathfinder(program: Array[Long]) {
    val inputQueue = mutable.Queue[Long]()
    val outputQueue = mutable.Queue[Long]()
    val machine = new Machine(program, inputQueue, outputQueue)
    val grid = mutable.Map[(Int, Int), Char]((0, 0) -> '.')
    val dirmap = Map(1 -> (0, 1), 2 -> (0, -1), 3 -> (-1, 0), 4 -> (1, 0))
    var p = (0, 0)
    var oxygen: Option[(Int, Int)] = None

    def tryMove(dir: Int): Boolean = {
      inputQueue.enqueue(dir.toLong)
      machine.run()
      if (outputQueue.isEmpty) return false
      val output = outputQueue.dequeue()
      val nextPos = (p._1 + dirmap(dir)._1, p._2 + dirmap(dir)._2)

      output match {
        case 0 =>
          grid(nextPos) = '#'
          false
        case 1 =>
          grid(nextPos) = '.'
          p = nextPos
          true
        case 2 =>
          grid(nextPos) = 'O'
          oxygen = Some(nextPos)
          p = nextPos
          true
        case _ => throw new IllegalArgumentException(s"Unknown output: $output")
      }
    }

     def explore(): Unit = {
      while (openSet().nonEmpty) {
        if (!openSet().contains(p)) {
          val nextPos = openSet().minBy(pos => manhattan(p, pos))
          shortestPath(p, nextPos).foreach(move =>
            if (!tryMove(move)) throw new IllegalStateException("Bad path")
          )
        }

        var foundUnexplored = false
        while (!foundUnexplored) {
          var unexploredDir: Option[Int] = None
          for (dir <- dirmap.keys) {
            val nextPos = (p._1 + dirmap(dir)._1, p._2 + dirmap(dir)._2)
            if (!grid.contains(nextPos)) {
              unexploredDir = Some(dir)
              foundUnexplored = true
            }
          }

          unexploredDir match {
            case Some(dir) =>
              if (!tryMove(dir)) {}
            case None =>
              foundUnexplored = true
          }
        }
      }
    }

    def openSet(): Set[(Int, Int)] = {
      grid.filter(_._2 != '#').filter { case (pos, _) =>
        dirmap.values.exists { case (dx, dy) =>
          !grid.contains((pos._1 + dx, pos._2 + dy))
        }
      }.keySet.toSet
    }

    def shortestPath(start: (Int, Int), end: (Int, Int)): List[Int] = {
      val queue = PriorityQueue[(Int, (Int, Int), List[Int])]()(Ordering.by(x => -x._1))
      queue.enqueue((0, start, List.empty[Int]))
      val visited = mutable.Set[(Int, Int)]()

      while (queue.nonEmpty) {
        val (dist, pos, path) = queue.dequeue()
        if (pos == end) return path.reverse
        if (visited.contains(pos)) {} else {
          visited.add(pos)
          for ((dir, delta) <- dirmap) {
            val nextPos = (pos._1 + delta._1, pos._2 + delta._2)
            if (grid.contains(nextPos) && grid(nextPos) != '#') {
              queue.enqueue((dist + 1, nextPos, dir :: path))
            }
          }
        }
      }
      throw new IllegalArgumentException("No path found")
    }

    def longestPath(start: (Int, Int)): Int = {
      val queue = PriorityQueue[(Int, (Int, Int))]()(Ordering.by(x => -x._1))
      queue.enqueue((0, start))
      val distances = mutable.Map[(Int, Int), Int](start -> 0)

      while (queue.nonEmpty) {
        val (dist, pos) = queue.dequeue()
        for (delta <- dirmap.values) {
          val nextPos = (pos._1 + delta._1, pos._2 + delta._2)
          if (grid.contains(nextPos) && grid(nextPos) != '#' && (!distances.contains(nextPos) || distances(nextPos) > dist + 1)) {
            distances(nextPos) = dist + 1
            queue.enqueue((dist + 1, nextPos))
          }
        }
      }

      distances.values.max
    }
  }

  def main(args: Array[String]): Unit = {
    val program = Source.fromFile("input.txt").getLines().next().split(",").map(_.toLong).toArray
    val pathfinder = new Pathfinder(program)
    pathfinder.explore()
    println(pathfinder.longestPath(pathfinder.oxygen.get))
  }
}
