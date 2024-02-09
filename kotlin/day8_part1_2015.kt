import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    var totalDiff = 0

    file.forEachLine {
        val line = it
        val codeLength = line.length
        val memoryLength = calculateMemoryLength(line)
        totalDiff += codeLength - memoryLength
    }

    println(totalDiff)
}

fun calculateMemoryLength(s: String): Int {
    var length = 0
    var inEscape = false
    var hexCount = 0

    for (i in 1 until s.length - 1) {
        when {
            hexCount > 0 -> hexCount--
            inEscape -> {
                if (s[i] == 'x') {
                    hexCount = 2
                }
                inEscape = false
                length++
            }
            s[i] == '\\' -> inEscape = true
            else -> length++
        }
    }
    return length
}