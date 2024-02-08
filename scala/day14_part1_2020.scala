object Main extends App {
  import scala.io.Source

  def applyMask(value: Long, mask: String): Long = {
    var result: Long = 0
    for (i <- 0 until 36) {
      val bitValue: Long = 1L << (35 - i)
      if (mask(i) == '1') {
        result |= bitValue
      } else if (mask(i) == 'X') {
        result |= (value & bitValue)
      }
    }
    result
  }

  val file = Source.fromFile("input.txt")
  var mask = ""
  var mem = Map[Long, Long]()

  val reMem = "mem\\[(\\d+)] = (\\d+)".r

  for (line <- file.getLines()) {
    if (line.startsWith("mask = ")) {
      mask = line.stripPrefix("mask = ")
    } else {
      line match {
        case reMem(address, value) => mem += (address.toLong -> applyMask(value.toLong, mask))
        case _ =>
      }
    }
  }

  val sum = mem.values.sum
  println(sum)

  file.close()
}