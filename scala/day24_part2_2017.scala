object Solution extends App {
  case class Component(a: Int, b: Int)

  var maxStrength = 0
  var maxLength = 0

  def findStrongestLongestBridge(components: List[Component], used: Array[Boolean], port: Int, strength: Int, length: Int): Unit = {
    if (length > maxLength || (length == maxLength && strength > maxStrength)) {
      maxStrength = strength
      maxLength = length
    }

    for (i <- components.indices) {
      val c = components(i)
      if (!used(i)) {
        if (c.a == port || c.b == port) {
          used(i) = true
          val nextPort = if (c.a == port) c.b else c.a
          findStrongestLongestBridge(components, used, nextPort, strength + c.a + c.b, length + 1)
          used(i) = false
        }
      }
    }
  }

  val source = scala.io.Source.fromFile("input.txt")
  val components = source.getLines().map { line =>
    val ports = line.split("/")
    Component(ports(0).toInt, ports(1).toInt)
  }.toList
  source.close()

  val used = Array.fill(components.length)(false)
  findStrongestLongestBridge(components, used, 0, 0, 0)

  println(maxStrength)
}