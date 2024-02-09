import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    val lengths = mutableListOf<Int>()
    input.forEach { lengths.add(it.toInt()) }
    lengths.addAll(listOf(17, 31, 73, 47, 23))

    val list = IntArray(256) { it }
    var currentPosition = 0
    var skipSize = 0

    repeat(64) {
        for (length in lengths) {
            for (i in 0 until length / 2) {
                val start = (currentPosition + i) % 256
                val end = (currentPosition + length - 1 - i) % 256
                val temp = list[start]
                list[start] = list[end]
                list[end] = temp
            }
            currentPosition = (currentPosition + length + skipSize) % 256
            skipSize++
        }
    }

    val denseHash = mutableListOf<Byte>()
    for (i in 0 until 256 step 16) {
        var xor = 0
        for (j in 0 until 16) {
            xor = xor xor list[i + j]
        }
        denseHash.add(xor.toByte())
    }

    val hexHash = denseHash.joinToString("") { String.format("%02x", it) }
    println(hexHash)
}