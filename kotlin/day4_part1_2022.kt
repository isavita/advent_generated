import java.io.File

fun parseRange(r: String): Pair<Int, Int> {
    val parts = r.split("-")
    val start = parts[0].toInt()
    val end = parts[1].toInt()
    return Pair(start, end)
}

fun main(args: Array<String>) {
    var count = 0

    File("input.txt").forEachLine {
        val ranges = it.split(",")
        if (ranges.size != 2) {
            return@forEachLine
        }
        val (start1, end1) = parseRange(ranges[0])
        val (start2, end2) = parseRange(ranges[1])

        if ((start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)) {
            count++
        }
    }

    println(count)
}