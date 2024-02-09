import java.io.File

fun main(args: Array<String>) {
    val inputFile = File("input.txt")
    val lines = inputFile.readLines()

    var count = 0
    for (line in lines) {
        val pair = line.split(",")

        // Extract ranges
        val left = parseRange(pair[0])
        val right = parseRange(pair[1])

        // Check if ranges overlap
        if (left[0] <= right[1] && left[1] >= right[0]) {
            count++
        }
    }

    println(count)
}

fun parseRange(s: String): IntArray {
    val split = s.split("-")
    val start = split[0].toInt()
    val end = split[1].toInt()
    return intArrayOf(start, end)
}