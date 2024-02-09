import java.io.File

fun reverseSection(arr: IntArray, start: Int, length: Int) {
    val n = arr.size
    for (i in start until start + length / 2) {
        val j = start + length - 1 - (i - start)
        val temp = arr[i % n]
        arr[i % n] = arr[j % n]
        arr[j % n] = temp
    }
}

fun knotHash(input: String): String {
    var lengths = input.map { it.toInt() }.toMutableList()
    lengths.addAll(listOf(17, 31, 73, 47, 23))

    var list = IntArray(256) { it }

    var position = 0
    var skip = 0
    repeat(64) {
        for (length in lengths) {
            reverseSection(list, position, length)
            position += length + skip
            skip++
        }
    }

    val denseHash = IntArray(16) { i ->
        list.slice(i * 16 until (i + 1) * 16).reduce { acc, num -> acc xor num }
    }

    return denseHash.joinToString("") { it.toString(16).padStart(2, '0') }
}

fun hexToBinary(hexStr: String): String {
    return hexStr.map {
        Integer.parseInt(it.toString(), 16).toString(2).padStart(4, '0')
    }.joinToString("")
}

fun main(args: Array<String>) {
    val keyString = File("input.txt").readText().trim()
    var totalUsed = 0

    repeat(128) { i ->
        val rowKey = "$keyString-$i"
        val hash = knotHash(rowKey)
        val binaryRow = hexToBinary(hash)

        totalUsed += binaryRow.count { it == '1' }
    }

    println(totalUsed)
}