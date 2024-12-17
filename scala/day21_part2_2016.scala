
import scala.io.Source
import scala.util.Using

object Main {

  class Scrambler(var pw: Array[Char]) {
    override def toString: String = pw.mkString

    def swapPositions(x: Int, y: Int): Unit = {
      val temp = pw(x)
      pw(x) = pw(y)
      pw(y) = temp
    }

    def swapLetters(x: Char, y: Char): Unit = {
      val xi = pw.indexOf(x)
      val yi = pw.indexOf(y)
      swapPositions(xi, yi)
    }

    def rotate(steps: Int): Unit = {
      val length = pw.length
      val s = (steps % length + length) % length
      pw = pw.slice(length - s, length) ++ pw.slice(0, length - s)
    }

    def rotateLetter(x: Char): Unit = {
      val index = pw.indexOf(x)
      val steps = if (index >= 4) index + 2 else index + 1
      rotate(steps)
    }

    def derotateLetter(x: Char): Unit = {
      val index = pw.indexOf(x)
      val rot = if (index % 2 == 1) -(index + 1) / 2
      else if (index != 0) (6 - index) / 2
      else -1
      rotate(rot)
    }

    def reverse(x: Int, y: Int): Unit = {
      var i = x
      var j = y
      while (i < j) {
        val temp = pw(i)
        pw(i) = pw(j)
        pw(j) = temp
        i += 1
        j -= 1
      }
    }

    def move(x: Int, y: Int): Unit = {
      val ch = pw(x)
      if (x < y) {
        System.arraycopy(pw, x + 1, pw, x, y - x)
      } else {
        System.arraycopy(pw, y, pw, y + 1, x - y)
      }
      pw(y) = ch
    }

    def scramble(instructions: List[String], direction: Int): Scrambler = {
      val inst = if (direction < 0) instructions.reverse else instructions
      inst.foreach { instruction =>
        val line = instruction.split(" ")
        line(0) match {
          case "swap" =>
            if (line(1) == "position") {
              swapPositions(line(2).toInt, line(5).toInt)
            } else {
              swapLetters(line(2)(0), line(5)(0))
            }
          case "rotate" =>
            if (line(1) == "based") {
              if (direction > 0) rotateLetter(line(6)(0))
              else derotateLetter(line(6)(0))
            } else {
              var x = line(2).toInt
              if (line(1) == "left") x = -x
              if (direction < 0) x = -x
              rotate(x)
            }
          case "reverse" =>
            reverse(line(2).toInt, line(4).toInt)
          case "move" =>
            var x = line(2).toInt
            var y = line(5).toInt
            if (direction < 0) {
              val temp = x
              x = y
              y = temp
            }
            move(x, y)
        }
      }
      this
    }

    def unscramble(instructions: List[String]): Scrambler = scramble(instructions, -1)
  }

  def main(args: Array[String]): Unit = {
    val instructions = Using(Source.fromFile("input.txt")) { source =>
      source.getLines().toList
    }.get

    val hashed = "fbgdceah"
    val scrambler = new Scrambler(hashed.toCharArray)
    val result = scrambler.unscramble(instructions)
    println(result)
  }
}
