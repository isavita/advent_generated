
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    val k = mutableListOf<Int>()
    val l = mutableListOf<Int>()
    val m = mutableListOf<Int>()

    lines.forEachIndexed { i, line ->
        when (i % 18) {
            4 -> l.add(line.substringAfter("div z ").toInt())
            5 -> k.add(line.substringAfter("add x ").toInt())
            15 -> m.add(line.substringAfter("add y ").toInt())
        }
    }

    val constraints = mutableMapOf<Int, Pair<Int, Int>>()
    val stack = mutableListOf<Int>()

    l.forEachIndexed { i, divZ ->
        when (divZ) {
            1 -> stack.add(i)
            26 -> {
                val pop = stack.removeLast()
                constraints[pop] = Pair(i, m[pop] + k[i])
            }
        }
    }

    val min = IntArray(14)
    for (i in 0 until 14) {
        val constraint = constraints[i] ?: continue
        var vmin = 1
        while (vmin + constraint.second < 1) {
            vmin++
        }
        min[i] = vmin
        min[constraint.first] = vmin + constraint.second
    }

    println(min.joinToString("").toLong())
}
