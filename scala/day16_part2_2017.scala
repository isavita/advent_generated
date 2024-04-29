object Dance {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").mkString
    var programs = ('a' to 'p').toArray
    val moves = input.split(",")

    def spin(x: Int): Unit = {
      val (left, right) = programs.splitAt(programs.length - x)
      programs = right ++ left
    }

    def exchange(a: Int, b: Int): Unit = {
      val temp = programs(a)
      programs(a) = programs(b)
      programs(b) = temp
    }

    def partner(a: Char, b: Char): Unit = {
      val ai = programs.indexOf(a)
      val bi = programs.indexOf(b)
      exchange(ai, bi)
    }

    for (_ <- 1 to 1000000000 % 60) {
      moves.foreach { move =>
        move(0) match {
          case 's' => spin(move.tail.toInt)
          case 'x' => {
            val Array(a, b) = move.tail.split("/").map(_.toInt)
            exchange(a, b)
          }
          case 'p' => {
            val Array(a, b) = move.tail.split("/").map(_.head)
            partner(a, b)
          }
        }
      }
    }

    println(programs.mkString)
  }
}