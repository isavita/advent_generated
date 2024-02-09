import java.io.File

fun main() {
    val input = File("input.txt").readLines()

    var totalCodeLength = 0
    var totalMemoryLength = 0
    var totalEncodedLength = 0

    input.forEach { line ->
        totalCodeLength += line.length
        totalMemoryLength += getMemoryLength(line)
        totalEncodedLength += getEncodedLength(line)
    }

    println(totalCodeLength - totalMemoryLength)
    println(totalEncodedLength - totalCodeLength)
}

fun getMemoryLength(line: String): Int {
    var count = 0
    var i = 1
    while (i < line.length - 1) {
        if (line[i] == '\\') {
            if (line[i + 1] == '\\' || line[i + 1] == '"') {
                count++
                i += 2
            } else if (line[i + 1] == 'x') {
                count++
                i += 4
            }
        } else {
            count++
            i++
        }
    }
    return count
}

fun getEncodedLength(line: String): Int {
    var count = 2 // for the surrounding double quotes
    line.forEach { char ->
        if (char == '\\' || char == '"') {
            count += 2
        } else {
            count++
        }
    }
    return count
}