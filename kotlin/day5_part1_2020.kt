import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val maxSeatID = file.readLines()
        .map { it.replace("F", "0").replace("B", "1").replace("L", "0").replace("R", "1") }
        .map { decode(it) }
        .maxOrNull()

    println(maxSeatID)
}

fun decode(pass: String): Int {
    val row = binaryToInt(pass.substring(0, 7))
    val column = binaryToInt(pass.substring(7))
    return row * 8 + column
}

fun binaryToInt(binaryStr: String): Int {
    var result = 0
    for ((index, char) in binaryStr.withIndex()) {
        if (char == '1') {
            result = result or (1 shl (binaryStr.length - index - 1))
        }
    }
    return result
}