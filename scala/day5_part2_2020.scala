object Solution extends App {
  import scala.io.Source

  val filename = "input.txt"
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  var seatIDs = List[Int]()

  def decode(pass: String): Int = {
    def binaryToInt(binaryStr: String): Int =
      binaryStr.zipWithIndex.foldLeft(0)((acc, pair) => if (pair._1 == '1') acc | (1 << (binaryStr.length - pair._2 - 1)) else acc)

    val row = binaryToInt(pass.take(7).replace('F', '0').replace('B', '1'))
    val column = binaryToInt(pass.takeRight(3).replace('L', '0').replace('R', '1'))
    row * 8 + column
  }

  lines.foreach { pass =>
    val seatID = decode(pass)
    seatIDs = seatID :: seatIDs
  }

  val sortedSeatIDs = seatIDs.sorted

  sortedSeatIDs.sliding(2).find(pair => pair(1) != pair(0) + 1) match {
    case Some(pair) => println(pair(0) + 1)
    case None => println("Seat not found")
  }
}