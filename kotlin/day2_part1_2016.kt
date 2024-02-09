import java.io.File

fun main(args: Array<String>) {
    val instructions = File("input.txt").readLines()
    val code = getBathroomCode(instructions)
    println(code)
}

fun getBathroomCode(instructions: List<String>): String {
    val keypad = arrayOf(
        intArrayOf(1, 2, 3),
        intArrayOf(4, 5, 6),
        intArrayOf(7, 8, 9)
    )
    var x = 1
    var y = 1
    var code = ""

    for (instruction in instructions) {
        for (move in instruction) {
            when (move) {
                'U' -> if (x > 0) x--
                'D' -> if (x < 2) x++
                'L' -> if (y > 0) y--
                'R' -> if (y < 2) y++
            }
        }
        code += keypad[x][y]
    }

    return code
}