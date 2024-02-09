import java.io.File

fun main(args: Array<String>) {
    val s = File("input.txt").readText().trim()
    println(firstNUnique(s, 4))
}

fun firstNUnique(s: String, n: Int): Int {
    for (i in n until s.length) {
        val b = s.substring(i - n, i).toSet()
        if (b.size == n) {
            return i
        }
    }
    return -1
}