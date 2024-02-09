import java.io.File

data class Scanner(var range: Int, var position: Int, var direction: Int)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val firewall = mutableMapOf<Int, Scanner>()

    file.forEachLine {
        val fields = it.split(": ")
        val depth = fields[0].toInt()
        val rng = fields[1].toInt()
        firewall[depth] = Scanner(rng, 0, 1)
    }

    var delay = 0
    while (!passThrough(firewall, delay)) {
        delay++
    }

    println(delay)
}

fun passThrough(firewall: Map<Int, Scanner>, delay: Int): Boolean {
    for ((depth, scanner) in firewall) {
        if ((depth + delay) % (2 * (scanner.range - 1)) == 0) {
            return false
        }
    }
    return true
}