import java.io.File

fun main() {
    val input = File("input.txt").readText()
    val packetMarkerLength = 4
    val messageMarkerLength = 14

    fun findMarker(input: String, markerLength: Int): Int {
        for (i in markerLength - 1 until input.length) {
            val marker = input.substring(i - markerLength + 1, i + 1)
            if (marker.toSet().size == markerLength) {
                return i + 1
            }
        }
        return -1
    }

    val packetMarkerPosition = findMarker(input, packetMarkerLength)
    val messageMarkerPosition = findMarker(input, messageMarkerLength)

    println("Part 1: $packetMarkerPosition")
    println("Part 2: $messageMarkerPosition")
}