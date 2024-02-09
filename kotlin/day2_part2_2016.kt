import java.io.File

fun main(args: Array<String>) {
    val instructions = File("input.txt").readLines()
    val code = getBathroomCode(instructions)
    println(code)
}

fun getBathroomCode(instructions: List<String>): String {
    val keypad = mapOf(
        "1" to mapOf('D' to "3"),
        "2" to mapOf('R' to "3", 'D' to "6"),
        "3" to mapOf('U' to "1", 'R' to "4", 'D' to "7", 'L' to "2"),
        "4" to mapOf('L' to "3", 'D' to "8"),
        "5" to mapOf('R' to "6"),
        "6" to mapOf('U' to "2", 'R' to "7", 'D' to "A", 'L' to "5"),
        "7" to mapOf('U' to "3", 'R' to "8", 'D' to "B", 'L' to "6"),
        "8" to mapOf('U' to "4", 'R' to "9", 'D' to "C", 'L' to "7"),
        "9" to mapOf('L' to "8"),
        "A" to mapOf('U' to "6", 'R' to "B"),
        "B" to mapOf('U' to "7", 'R' to "C", 'D' to "D", 'L' to "A"),
        "C" to mapOf('U' to "8", 'L' to "B"),
        "D" to mapOf('U' to "B")
    )

    var position = "5"
    var code = ""

    for (instruction in instructions) {
        for (move in instruction) {
            keypad[position]?.get(move)?.let { nextPos ->
                position = nextPos
            }
        }
        code += position
    }

    return code
}