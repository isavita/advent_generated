import java.io.File

fun main(args: Array<String>) {
    val masses = mutableListOf<Int>()
    var total = 0.0

    File("input.txt").forEachLine {
        masses.add(it.trim().toInt())
    }

    for (mass in masses) {
        total += (Math.floor(mass.toDouble() / 3) - 2)
    }

    println(total)
}