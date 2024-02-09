import java.io.File
import java.util.regex.Pattern

data class Disc(val totalPositions: Int, val startPosition: Int)

fun main(args: Array<String>) {
    val discs = mutableListOf<Disc>()
    val discRegex = Pattern.compile("Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+).")

    File("input.txt").forEachLine { line ->
        val matcher = discRegex.matcher(line)
        if (matcher.find()) {
            val totalPositions = matcher.group(2).toInt()
            val startPosition = matcher.group(3).toInt()
            discs.add(Disc(totalPositions, startPosition))
        }
    }

    discs.add(Disc(11, 0))

    var time = 0
    while (true) {
        if (checkDiscs(discs, time)) {
            println(time)
            break
        }
        time++
    }
}

fun checkDiscs(discs: List<Disc>, time: Int): Boolean {
    discs.forEachIndexed { index, disc ->
        val position = (disc.startPosition + time + index + 1) % disc.totalPositions
        if (position != 0) {
            return false
        }
    }
    return true
}