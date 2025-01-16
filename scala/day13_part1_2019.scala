
import scala.io.Source
import scala.collection.mutable

object Solution {
  def main(args: Array[String]): Unit = {
    val program = Source.fromFile("input.txt").mkString.trim.split(",").map(_.toInt)
    println(countBlocks(program))
  }

  def countBlocks(program: Array[Int]): Int = {
    val grid = mutable.Map.empty[(Int, Int), Int]
    val out = run(program, Iterator.empty)
    out.grouped(3).foreach { case Seq(x, y, t) => grid((x, y)) = t }
    grid.values.count(_ == 2)
  }

  def run(program: Array[Int], in: Iterator[Int]): List[Int] = {
    val data = mutable.Map.empty[Int, Int].withDefaultValue(0)
    program.zipWithIndex.foreach { case (v, i) => data(i) = v }
    var ip = 0
    var relBase = 0
    val out = mutable.ListBuffer.empty[Int]

    def get(i: Int, mode: Int): Int = mode match {
      case 0 => data(data(i))
      case 1 => data(i)
      case 2 => data(relBase + data(i))
    }

    def set(i: Int, mode: Int, value: Int): Unit = mode match {
      case 0 => data(data(i)) = value
      case 2 => data(relBase + data(i)) = value
    }

    while (data(ip) != 99) {
      val op = data(ip) % 100
      val modes = Seq(data(ip) / 100 % 10, data(ip) / 1000 % 10, data(ip) / 10000 % 10)
      op match {
        case 1 =>
          set(ip + 3, modes(2), get(ip + 1, modes(0)) + get(ip + 2, modes(1)))
          ip += 4
        case 2 =>
          set(ip + 3, modes(2), get(ip + 1, modes(0)) * get(ip + 2, modes(1)))
          ip += 4
        case 3 =>
          set(ip + 1, modes(0), in.next())
          ip += 2
        case 4 =>
          out += get(ip + 1, modes(0))
          ip += 2
        case 5 =>
          ip = if (get(ip + 1, modes(0)) != 0) get(ip + 2, modes(1)) else ip + 3
        case 6 =>
          ip = if (get(ip + 1, modes(0)) == 0) get(ip + 2, modes(1)) else ip + 3
        case 7 =>
          set(ip + 3, modes(2), if (get(ip + 1, modes(0)) < get(ip + 2, modes(1))) 1 else 0)
          ip += 4
        case 8 =>
          set(ip + 3, modes(2), if (get(ip + 1, modes(0)) == get(ip + 2, modes(1))) 1 else 0)
          ip += 4
        case 9 =>
          relBase += get(ip + 1, modes(0))
          ip += 2
      }
    }
    out.toList
  }
}
