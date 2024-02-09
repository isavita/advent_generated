import java.io.File

data class Disc(val totalPositions: Int, val startPosition: Int)

fun main(args: Array<String>) {
    val discs = mutableListOf<Disc>()
    File("input.txt").forEachLine {
        val regexMatch = Regex("""Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).""").find(it)
        val totalPositions = regexMatch!!.groupValues[2].toInt()
        val startPosition = regexMatch.groupValues[3].toInt()
        discs.add(Disc(totalPositions, startPosition))
    }

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