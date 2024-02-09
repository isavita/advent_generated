import java.io.File

fun main(args: Array<String>) {
    val x = mutableListOf(1)
    File("input.txt").forEachLine {
        when {
            it == "noop" -> x.add(x.last())
            else -> {
                val n = it.substringAfter("addx ").toInt()
                x.add(x.last())
                x.add(x.last() + n)
            }
        }
    }

    var sum = 0
    for (i in x.indices) {
        if ((i - 19) % 40 == 0) {
            sum += (i + 1) * x[i]
        }
    }
    println(sum)
}