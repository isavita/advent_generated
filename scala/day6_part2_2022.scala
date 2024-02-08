
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next()

  def findMarkerLength(str: String, markerLength: Int): Int = {
    str.sliding(markerLength).indexWhere(chunk => chunk.toSet.size == markerLength) + markerLength
  }

  val packetMarkerLength = 4
  val messageMarkerLength = 14

  val firstPacketMarker = findMarkerLength(input, packetMarkerLength)
  val firstMessageMarker = findMarkerLength(input, messageMarkerLength)

  println(firstPacketMarker)
  println(firstMessageMarker)
}
