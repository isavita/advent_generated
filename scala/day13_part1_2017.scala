
object Day13 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val firewall = input.map(_.split(": ").map(_.toInt))
  val severity = firewall.map(layer => if (layer(0) % (2 * (layer(1) - 1)) == 0) layer(0) * layer(1) else 0).sum
  println(severity)
}
