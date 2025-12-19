import java.io.{BufferedReader, FileReader}
import java.math.BigInteger
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val br = new BufferedReader(new FileReader("input.txt"))
    val grid = Iterator.continually(br.readLine()).takeWhile(_ != null).filter(_.nonEmpty).toArray
    br.close()
    if (grid.isEmpty) {
      println("0")
      return
    }
    val height = grid.length
    val width = grid(0).length
    var startX = -1
    var startY = -1
    var found = false
    var y = 0
    while (y < height && !found) {
      val row = grid(y)
      var x = 0
      while (x < width && !found) {
        if (row.charAt(x) == 'S') {
          startX = x
          startY = y
          found = true
        }
        x += 1
      }
      y += 1
    }
    if (!found) {
      System.err.println("Start point 'S' not found")
      return
    }
    val counts = mutable.HashMap[Int, BigInteger]()
    counts(startX) = BigInteger.ONE
    var curY = startY
    while (curY < height) {
      val next = mutable.HashMap[Int, BigInteger]()
      for ((x, cnt) <- counts) {
        val isSplitter = x >= 0 && x < width && grid(curY).charAt(x) == '^'
        if (isSplitter) {
          add(next, x - 1, cnt)
          add(next, x + 1, cnt)
        } else {
          add(next, x, cnt)
        }
      }
      counts.clear()
      counts ++= next
      curY += 1
    }
    var total = BigInteger.ZERO
    for (cnt <- counts.values) total = total.add(cnt)
    println(total.toString)
  }

  private def add(m: mutable.HashMap[Int, BigInteger], key: Int, value: BigInteger): Unit = {
    m.update(key, m.getOrElse(key, BigInteger.ZERO).add(value))
  }
}