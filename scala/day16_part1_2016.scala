
object DragonChecksum extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().mkString
  def dragonCurve(s: String): String = s + "0" + s.reverse.map(c => if (c == '0') '1' else '0')
  def checksum(s: String): String = if (s.length % 2 == 1) s else checksum(s.grouped(2).map(pair => if (pair(0) == pair(1)) '1' else '0').mkString)
  var data = input
  while (data.length < 272) {
    data = dragonCurve(data)
  }
  data = data.substring(0, 272)
  println(checksum(data))
}
