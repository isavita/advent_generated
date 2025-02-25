
import scala.io.Source

object Solution {

  case class Complex(real: Int, imag: Int) {
    def +(other: Complex): Complex = Complex(real + other.real, imag + other.imag)
  }

  def solve(inputStr: String): Int = {
    val blocks = inputStr.trim.split("\n\n")
    val lines = blocks(0).split("\n")
    var m = Map[Complex, Char]()
    for ((row, y) <- lines.zipWithIndex) {
      for ((char, x) <- row.zipWithIndex) {
        m += (Complex(x, y) -> char)
      }
    }

    val steps = blocks(1).replace("\n", "").map {
      case '^' => Complex(0, -1)
      case '<' => Complex(-1, 0)
      case '>' => Complex(1, 0)
      case 'v' => Complex(0, 1)
    }

    var robot = m.find(_._2 == '@').get._1

    def tryToStep(m: Map[Complex, Char], pos: Complex, dir: Complex): (Boolean, Map[Complex, Char]) = {
      val orig = m
      m.get(pos) match {
        case Some('.') => (true, m)
        case Some('O') | Some('@') =>
          val (success, nextM) = tryToStep(m, pos + dir, dir)
          if (success) {
            val updatedM = nextM + (pos + dir -> nextM(pos)) + (pos -> '.')
            (true, updatedM)
          } else (false, orig)
        case Some(']') =>
          val (success, nextM) = tryToStep(m, pos + Complex(-1, 0), dir)
          (success, if (success) nextM else orig)
        case Some('[') =>
          if (dir == Complex(-1, 0)) {
            val (success, nextM) = tryToStep(m, pos + Complex(-1, 0), dir)
            if (success) {
              val updatedM = nextM + (pos + Complex(-1, 0) -> '[') + (pos -> ']') + (pos + Complex(1, 0) -> '.')
              (true, updatedM)
            } else (false, orig)
          } else if (dir == Complex(1, 0)) {
            val (success, nextM) = tryToStep(m, pos + Complex(2, 0), dir)
            if (success) {
              val updatedM = nextM + (pos -> '.') + (pos + Complex(1, 0) -> '[') + (pos + Complex(2, 0) -> ']')
              (true, updatedM)
            } else (false, orig)
          } else {
            val (success1, nextM1) = tryToStep(m, pos + dir, dir)
            if (success1) {
              val (success2, nextM2) = tryToStep(nextM1, pos + Complex(1, 0) + dir, dir)
              if (success2) {
                val updatedM = nextM2 + (pos -> '.') + (pos + Complex(1, 0) -> '.') + (pos + dir -> '[') + (pos + dir + Complex(1, 0) -> ']')
                (true, updatedM)
              } else (false, orig)
            } else (false, orig)
          }
        case _ => (false, orig)
      }
    }

    var currentM = m
    for (dir <- steps) {
      val (success, nextM) = tryToStep(currentM, robot, dir)
      if (success) {
        robot = robot + dir
        currentM = nextM
      }
    }

    currentM.filter(p => p._2 == '[' || p._2 == 'O').keys.map(k => k.real + 100 * k.imag).sum
  }

    def scaleUp(inputStr: String): String = {
      inputStr.replace("#", "##").replace(".", "..").replace("O", "[]").replace("@", "@.")
    }

  def main(args: Array[String]): Unit = {
    val inputStr = Source.fromFile("input.txt").mkString
    println(solve(inputStr))
     println(solve(scaleUp(inputStr)))
  }
}
