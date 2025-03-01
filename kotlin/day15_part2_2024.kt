
import java.io.File

data class Complex(val x: Int, val y: Int) {
    operator fun plus(other: Complex) = Complex(x + other.x, y + other.y)
}

fun solve(inputStr: String): Int {
    val blocks = inputStr.trim().split("\n\n")
    val lines = blocks[0].split("\n")
    val m = mutableMapOf<Complex, Char>()
    lines.forEachIndexed { y, row ->
        row.forEachIndexed { x, char ->
            m[Complex(x, y)] = char
        }
    }
    val steps = blocks[1].replace("\n", "").map {
        when (it) {
            '^' -> Complex(0, -1)
            '<' -> Complex(-1, 0)
            '>' -> Complex(1, 0)
            'v' -> Complex(0, 1)
            else -> throw IllegalArgumentException()
        }
    }
    var robot = m.entries.first { it.value == '@' }.key
    for (dir in steps) {
        if (tryToStep(m, robot, dir)) {
            robot += dir
        }
    }
    return m.entries.filter { it.value in listOf('[', 'O') }.sumOf { it.key.x + 100 * it.key.y }
}

fun tryToStep(m: MutableMap<Complex, Char>, pos: Complex, dir: Complex): Boolean {
    val orig = m.toMap()
    when (m[pos]) {
        '.' -> return true
        'O', '@' -> {
            if (tryToStep(m, pos + dir, dir)) {
                m[pos + dir] = m[pos]!!
                m[pos] = '.'
                return true
            }
        }
        ']' -> {
            if (tryToStep(m, pos + Complex(-1, 0), dir)) {
                return true
            }
        }
        '[' -> {
            when (dir) {
                Complex(-1, 0) -> {
                    if (tryToStep(m, pos + Complex(-1, 0), dir)) {
                        m[pos + Complex(-1, 0)] = '['
                        m[pos] = ']'
                        m[pos + Complex(1, 0)] = '.'
                        return true
                    }
                }
                Complex(1, 0) -> {
                    if (tryToStep(m, pos + Complex(2, 0), dir)) {
                        m[pos] = '.'
                        m[pos + Complex(1, 0)] = '['
                        m[pos + Complex(2, 0)] = ']'
                        return true
                    }
                }
                else -> {
                    if (tryToStep(m, pos + dir, dir) && tryToStep(m, pos + Complex(1, 0) + dir, dir)) {
                        m[pos] = '.'
                        m[pos + Complex(1, 0)] = '.'
                        m[pos + dir] = '['
                        m[pos + dir + Complex(1, 0)] = ']'
                        return true
                    }
                }
            }
        }
    }
    m.clear()
    m.putAll(orig)
    return false
}

fun scaleUp(inputStr: String): String {
    return inputStr.replace("#", "##")
        .replace(".", "..")
        .replace("O", "[]")
        .replace("@", "@.")
}

fun main() {
    val inputStr = File("input.txt").readText()
    println(solve(inputStr))
    println(solve(scaleUp(inputStr)))
}
