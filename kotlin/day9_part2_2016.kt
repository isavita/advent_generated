import java.io.File

fun main() {
    val input = File("input.txt").readText().replace("\\s+".toRegex(), "")

    println("Part 1: ${decompress(input).length}")

    println("Part 2: ${decompressV2(input)}")
}

fun decompress(input: String): String {
    var result = ""
    var i = 0
    while (i < input.length) {
        if (input[i] == '(') {
            val markerEnd = input.indexOf(')', i)
            val marker = input.substring(i + 1, markerEnd).split('x')
            val length = marker[0].toInt()
            val repeat = marker[1].toInt()
            val substr = input.substring(markerEnd + 1, markerEnd + length + 1)
            result += substr.repeat(repeat)
            i = markerEnd + length + 1
        } else {
            result += input[i]
            i++
        }
    }
    return result
}

fun decompressV2(input: String): Long {
    var result = 0L
    var i = 0
    while (i < input.length) {
        if (input[i] == '(') {
            val markerEnd = input.indexOf(')', i)
            val marker = input.substring(i + 1, markerEnd).split('x')
            val length = marker[0].toInt()
            val repeat = marker[1].toInt()
            val substr = input.substring(markerEnd + 1, markerEnd + length + 1)
            result += repeat * decompressV2(substr)
            i = markerEnd + length + 1
        } else {
            result++
            i++
        }
    }
    return result
}