import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()[0].split(",").map { it.toInt() }
    
    val list = (0 until 256).toList().toMutableList()
    var currentPosition = 0
    var skipSize = 0

    input.forEach { length ->
        for (i in 0 until length / 2) {
            val start = (currentPosition + i) % 256
            val end = (currentPosition + length - 1 - i) % 256
            list[start] = list[end].also { list[end] = list[start] }
        }

        currentPosition = (currentPosition + length + skipSize) % 256
        skipSize++
    }

    val result = list[0] * list[1]
    println(result)
}