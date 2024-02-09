import java.io.File

fun main(args: Array<String>) {
    val instructions = File("input.txt").readLines()
    val result = executeBootCode(instructions)
    println(result.first)
}

fun executeBootCode(instructions: List<String>): Pair<Int, Boolean> {
    var accumulator = 0
    val visited = mutableMapOf<Int, Boolean>()
    var currentInstruction = 0

    while (currentInstruction < instructions.size) {
        if (visited[currentInstruction] == true) {
            return Pair(accumulator, true)
        }

        visited[currentInstruction] = true
        val parts = instructions[currentInstruction].split(" ")
        val op = parts[0]
        val arg = parts[1].toInt()

        when (op) {
            "acc" -> {
                accumulator += arg
                currentInstruction++
            }
            "jmp" -> currentInstruction += arg
            "nop" -> currentInstruction++
        }
    }

    return Pair(accumulator, false)
}