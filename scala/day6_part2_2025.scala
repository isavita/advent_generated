
import java.nio.file.{Files, Paths}
import java.math.BigInteger
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Files.readAllLines(Paths.get("input.txt")).toArray.map(_.toString)
    if (lines.isEmpty) { println("Grand total: 0"); return }

    val maxWidth = lines.map(_.length).max
    val isSep = Array.fill(maxWidth)(true)
    for (x <- 0 until maxWidth) {
      var allSpace = true
      var i = 0
      while (i < lines.length && allSpace) {
        if (x < lines(i).length && !lines(i)(x).isWhitespace) allSpace = false
        i += 1
      }
      isSep(x) = allSpace
    }

    var grandTotal = BigInteger.ZERO
    var inBlock = false
    var startCol = 0

    for (x <- 0 until maxWidth) {
      if (!isSep(x)) {
        if (!inBlock) { inBlock = true; startCol = x }
      } else if (inBlock) {
        grandTotal = grandTotal.add(processBlock(lines, startCol, x - 1))
        inBlock = false
      }
    }
    if (inBlock) grandTotal = grandTotal.add(processBlock(lines, startCol, maxWidth - 1))

    println(s"Grand total: $grandTotal")
  }

  def processBlock(lines: Array[String], start: Int, end: Int): BigInteger = {
    val nums = new ArrayBuffer[BigInteger]()
    var operator = '*'
    for (c <- start to end) {
      val sb = new StringBuilder
      var hasDigits = false
      var r = 0
      while (r < lines.length) {
        if (c < lines(r).length) {
          lines(r)(c) match {
            case d if d.isDigit => sb.append(d); hasDigits = true
            case '+' => operator = '+'
            case '*' => operator = '*'
            case _ => ()
          }
        }
        r += 1
      }
      if (hasDigits) nums += new BigInteger(sb.toString())
    }
    if (nums.isEmpty) return BigInteger.ZERO
    if (operator == '*') {
      var prod = BigInteger.ONE
      nums.foreach(n => prod = prod.multiply(n))
      prod
    } else {
      var sum = BigInteger.ZERO
      nums.foreach(n => sum = sum.add(n))
      sum
    }
  }
}
