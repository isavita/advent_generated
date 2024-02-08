object Solution extends App {
  import scala.io.Source
  import scala.collection.mutable.ArrayBuffer

  def reverseSection(arr: ArrayBuffer[Int], start: Int, length: Int): Unit = {
    val n = arr.length
    for (i <- 0 until length / 2) {
      val tmp = arr((start + i) % n)
      arr((start + i) % n) = arr((start + length - i - 1) % n)
      arr((start + length - i - 1) % n) = tmp
    }
  }

  def knotHash(input: String): String = {
    var lengths = input.map(_.toInt).toArray ++ Array(17, 31, 73, 47, 23)
    var list = ArrayBuffer.range(0, 256)

    var position = 0
    var skip = 0
    for (_ <- 0 until 64) {
      for (length <- lengths) {
        reverseSection(list, position, length)
        position = (position + length + skip) % 256
        skip += 1
      }
    }

    val denseHash = list.grouped(16).map(_.reduce(_ ^ _)).toArray
    val hexHash = denseHash.map(_.toByte).toArray
    hexHash.map("%02x".format(_)).mkString
  }

  def hexToBinary(hexStr: String): String = {
    hexStr.flatMap(hexDigit => "%04d".format(Integer.parseInt(hexDigit.toString, 16).toBinaryString.toInt))
  }

  def dfs(x: Int, y: Int, grid: Array[Array[Int]]): Unit = {
    if (x < 0 || x >= 128 || y < 0 || y >= 128 || grid(x)(y) != 1) return
    grid(x)(y) = 0
    dfs(x - 1, y, grid)
    dfs(x + 1, y, grid)
    dfs(x, y - 1, grid)
    dfs(x, y + 1, grid)
  }

  val source = Source.fromFile("input.txt")
  val keyString = source.getLines().next()
  source.close()

  val grid = Array.ofDim[Int](128, 128)
  var totalUsed = 0
  var regions = 0

  for (i <- 0 until 128) {
    val rowKey = s"$keyString-$i"
    val hash = knotHash(rowKey)
    val binaryRow = hexToBinary(hash)

    for (j <- binaryRow.indices) {
      if (binaryRow(j) == '1') {
        grid(i)(j) = 1
        totalUsed += 1
      }
    }
  }

  for (i <- 0 until 128) {
    for (j <- 0 until 128) {
      if (grid(i)(j) == 1) {
        regions += 1
        dfs(i, j, grid)
      }
    }
  }

  println(regions)
}