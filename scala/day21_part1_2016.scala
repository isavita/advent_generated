import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val operations = input.split("\n")
    var password = "abcdefgh"

    for (op <- operations) {
      password = applyOperation(op, password)
    }

    println(password)
  }

  def applyOperation(op: String, password: String): String = {
    val fields = op.split(" ")
    fields(0) match {
      case "swap" =>
        fields(1) match {
          case "position" =>
            val x = fields(2).toInt
            val y = fields(5).toInt
            swapPosition(password, x, y)
          case "letter" =>
            val x = fields(2).head
            val y = fields(5).head
            swapLetter(password, x, y)
        }
      case "rotate" =>
        fields(1) match {
          case "left" =>
            val steps = fields(2).toInt
            rotateLeft(password, steps)
          case "right" =>
            val steps = fields(2).toInt
            rotateRight(password, steps)
          case "based" =>
            val x = fields(6).head
            rotateBasedOnPosition(password, x)
        }
      case "reverse" =>
        val x = fields(2).toInt
        val y = fields(4).toInt
        reversePositions(password, x, y)
      case "move" =>
        val x = fields(2).toInt
        val y = fields(5).toInt
        movePosition(password, x, y)
    }
  }

  def swapPosition(password: String, x: Int, y: Int): String = {
    val chars = password.toArray
    val temp = chars(x)
    chars(x) = chars(y)
    chars(y) = temp
    chars.mkString
  }

  def swapLetter(password: String, x: Char, y: Char): String = {
    password.map(c => if (c == x) y else if (c == y) x else c).mkString
  }

  def rotateLeft(password: String, steps: Int): String = {
    val stepsMod = steps % password.length
    password.drop(stepsMod) + password.take(stepsMod)
  }

  def rotateRight(password: String, steps: Int): String = {
    val stepsMod = steps % password.length
    password.drop(password.length - stepsMod) + password.take(password.length - stepsMod)
  }

  def rotateBasedOnPosition(password: String, x: Char): String = {
    val index = password.indexOf(x)
    val steps = (index + 1) + (if (index >= 4) 1 else 0)
    rotateRight(password, steps)
  }

  def reversePositions(password: String, x: Int, y: Int): String = {
    val chars = password.toArray
    var i = x
    var j = y
    while (i < j) {
      val temp = chars(i)
      chars(i) = chars(j)
      chars(j) = temp
      i += 1
      j -= 1
    }
    chars.mkString
  }

  def movePosition(password: String, x: Int, y: Int): String = {
    val chars = password.toArray
    val r = chars(x)
    var charsArray = chars.toBuffer
    charsArray.remove(x)
    charsArray.insert(y, r)
    charsArray.mkString
  }
}