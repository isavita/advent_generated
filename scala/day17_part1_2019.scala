
import scala.io.Source
import scala.collection.mutable

object Day17 {

  def decode(n: Long): (Int, List[Int]) = {
    val op = (n % 100).toInt
    val modes = List((n / 100).toInt % 10, (n / 1000).toInt % 10, (n / 10000).toInt % 10)
    (op, modes)
  }

  class Machine(program: Array[Long], inStream: Iterator[Long], outStream: mutable.ListBuffer[Long]) {
    val data: mutable.Map[Long, Long] = mutable.Map.from(program.zipWithIndex.map { case (v, i) => i.toLong -> v })
    var ip: Long = 0
    var relbase: Long = 0

    def get(i: Long, mo: Int): Long = {
      mo match {
        case 0 => data.getOrElse(data.getOrElse(i, 0L), 0L)
        case 1 => data.getOrElse(i, 0L)
        case 2 => data.getOrElse(relbase + data.getOrElse(i, 0L), 0L)
      }
    }

    def set(i: Long, mo: Int, value: Long): Unit = {
      mo match {
        case 0 => data(data(i)) = value
        case 2 => data(relbase + data(i)) = value
      }
    }

    def step(): Boolean = {
      val (op, modes) = decode(data.getOrElse(ip, 0L))
      op match {
        case 1 =>
          set(ip + 3, modes(2), get(ip + 1, modes(0)) + get(ip + 2, modes(1)))
          ip += 4
        case 2 =>
          set(ip + 3, modes(2), get(ip + 1, modes(0)) * get(ip + 2, modes(1)))
          ip += 4
        case 3 =>
          set(ip + 1, modes(0), inStream.next())
          ip += 2
        case 4 =>
          outStream.append(get(ip + 1, modes(0)))
          ip += 2
        case 5 =>
          if (get(ip + 1, modes(0)) != 0) ip = get(ip + 2, modes(1))
          else ip += 3
        case 6 =>
          if (get(ip + 1, modes(0)) == 0) ip = get(ip + 2, modes(1))
          else ip += 3
        case 7 =>
          set(ip + 3, modes(2), if (get(ip + 1, modes(0)) < get(ip + 2, modes(1))) 1 else 0)
          ip += 4
        case 8 =>
          set(ip + 3, modes(2), if (get(ip + 1, modes(0)) == get(ip + 2, modes(1))) 1 else 0)
          ip += 4
        case 9 =>
          relbase += get(ip + 1, modes(0))
          ip += 2
        case 99 =>
          return false
      }
      true
    }

    def run(): Unit = {
      while (step()) {}
    }
  }

  def run(program: Array[Long], inStream: Iterator[Long]): mutable.ListBuffer[Long] = {
    val outStream = mutable.ListBuffer.empty[Long]
    val machine = new Machine(program, inStream, outStream)
    machine.run()
    outStream
  }

  def parse(program: Array[Long]): (mutable.Map[(Int, Int), Char], (Int, Int), Int) = {
    val out = run(program, Iterator.empty)
    val scaffolding = mutable.Map.empty[(Int, Int), Char]
    var robot: (Int, Int) = (0, 0)
    var dir: Int = 0
    var x = 0
    var y = 0

    for (o <- out) {
      val c = o.toChar
      if (c == '\n') {
        y += 1
        x = 0
      } else {
        if ("^v<>".contains(c)) {
          robot = (x, y)
          dir = "^>v<".indexOf(c)
          scaffolding((x, y)) = '#'
        } else if (c == '#') {
          scaffolding((x, y)) = '#'
        }
        x += 1
      }
    }
    (scaffolding, robot, dir)
  }

  def sumAlign(grid: mutable.Map[(Int, Int), Char]): Int = {
    grid.keys.filter { case (x, y) =>
      List((0, 1), (0, -1), (1, 0), (-1, 0)).forall { case (dx, dy) => grid.contains((x + dx, y + dy)) }
    }.map { case (x, y) => x * y }.sum
  }

  def main(args: Array[String]): Unit = {
    val program = Source.fromFile("input.txt").mkString.trim.split(",").map(_.toLong)
    val (scaffolding, _, _) = parse(program)
    println(sumAlign(scaffolding))
  }
}
