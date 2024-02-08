
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next()
  
  def dragonCurve(s: String): String = {
    val b = s.reverse.map(c => if (c == '0') '1' else '0')
    s + "0" + b
  }
  
  def checksum(s: String): String = {
    if (s.length % 2 == 1) s
    else checksum(s.grouped(2).map(pair => if (pair(0) == pair(1)) '1' else '0').mkString)
  }
  
  def fillDisk(length: Int, initial: String): String = {
    LazyList.iterate(initial)(dragonCurve).find(_.length >= length).get.take(length)
  }
  
  val checksum1 = checksum(fillDisk(272, input))
  val checksum2 = checksum(fillDisk(35651584, input))
  
  println(checksum1)
  println(checksum2)
}
