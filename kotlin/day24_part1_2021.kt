import java.io.File

fun main() {
    val (k, l, m) = parseInput("input.txt")
    val constraints = mutableMapOf<Int, Pair<Int, Int>>()
    val stack = mutableListOf<Int>()
    
    for (i in l.indices) {
        when (l[i]) {
            1 -> stack.add(i)
            26 -> {
                val pop = stack.removeAt(stack.lastIndex)
                constraints[pop] = Pair(i, m[pop] + k[i])
            }
        }
    }

    val max = LongArray(14)
    for (i in 0 until 14) {
        constraints[i]?.let {
            var vmax = 9
            while (vmax + it.second > 9) vmax--
            max[i] = vmax.toLong()
            max[it.first] = (vmax + it.second).toLong()
        }
    }
    
    println(num(max))
}

fun num(w: LongArray): Long {
    return w.joinToString("").toLong()
}

fun parseInput(fileName: String): Triple<IntArray, IntArray, IntArray> {
    val lines = File(fileName).readLines()
    val k = mutableListOf<Int>()
    val l = mutableListOf<Int>()
    val m = mutableListOf<Int>()

    for (i in lines.indices) {
        val line = lines[i]
        when (i % 18) {
            4 -> l.add(line.split(" ")[2].toInt())
            5 -> k.add(line.split(" ")[2].toInt())
            15 -> m.add(line.split(" ")[2].toInt())
        }
    }
    
    return Triple(k.toIntArray(), l.toIntArray(), m.toIntArray())
}