import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()
    var sum = 0

    for (i in input.indices) {
        val next = (i + 1) % input.length
        if (input[i] == input[next]) {
            sum += input[i].toString().toInt()
        }
    }

    println(sum)
}