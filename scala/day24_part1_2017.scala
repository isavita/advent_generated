object Solution extends App {
  case class Component(a: Int, b: Int)

  var maxStrength = 0

  def findStrongestBridge(components: Array[Component], used: Array[Boolean], port: Int, strength: Int): Unit = {
    if (strength > maxStrength) {
      maxStrength = strength
    }

    for (i <- components.indices) {
      val c = components(i)
      if (used(i)) {
        // do nothing
      } else if (c.a == port || c.b == port) {
        used(i) = true
        val nextPort = if (c.a == port) c.b else c.a
        findStrongestBridge(components, used, nextPort, strength + c.a + c.b)
        used(i) = false
      }
    }
  }

  val source = scala.io.Source.fromFile("input.txt")
  val lines = source.getLines().toList
  source.close()

  val components = lines.map { line =>
    val ports = line.split("/")
    Component(ports(0).toInt, ports(1).toInt)
  }.toArray

  val used = Array.fill(components.length)(false)
  findStrongestBridge(components, used, 0, 0)

  println(maxStrength)
}