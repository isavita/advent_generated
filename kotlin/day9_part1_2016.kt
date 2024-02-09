import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()
    val decompressedLength = getDecompressedLength(input)
    println(decompressedLength)
}

fun getDecompressedLength(input: String): Int {
    val markerRegex = Regex("""\((\d+)x(\d+)\)""")
    var length = 0
    var i = 0
    while (i < input.length) {
        val marker = markerRegex.find(input, i)
        if (marker != null) {
            val (charCount, repeatCount) = marker.destructured
            val count = charCount.toInt() * repeatCount.toInt()
            length += count
            i = marker.range.last + 1 + charCount.toInt()
        } else {
            length++
            i++
        }
    }
    return length
}