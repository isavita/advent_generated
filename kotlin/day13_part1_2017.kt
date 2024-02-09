import java.io.File

data class Scanner(var range: Int, var position: Int, var direction: Int)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val firewall = mutableMapOf<Int, Scanner>()

    file.forEachLine {
        val fields = it.split(": ")
        val depth = fields[0].toInt()
        val range = fields[1].toInt()
        firewall[depth] = Scanner(range, 0, 1)
    }

    var severity = 0

    for (depth in 0..maxDepth(firewall)) {
        firewall[depth]?.let { scanner ->
            if (scanner.position == 0) {
                severity += depth * scanner.range
            }
        }

        firewall.values.forEach { moveScanner(it) }
    }

    println(severity)
}

fun maxDepth(firewall: Map<Int, Scanner>): Int {
    return firewall.keys.maxOrNull() ?: 0
}

fun moveScanner(scanner: Scanner) {
    if (scanner.position == 0) {
        scanner.direction = 1
    } else if (scanner.position == scanner.range - 1) {
        scanner.direction = -1
    }
    scanner.position += scanner.direction
}