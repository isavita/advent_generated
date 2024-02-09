import java.io.File

data class Instruction(val left: String, val right: String)

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    val re = Regex("[A-Z]{3}")

    val lines = input.split("\n")

    val desertMap = mutableMapOf<String, Instruction>()

    for (line in lines.drop(2)) {
        if (line.isBlank()) {
            continue
        }

        val matches = re.findAll(line).map { it.value }.toList()
        desertMap[matches[0]] = Instruction(
            left = matches[1],
            right = matches[2]
        )
    }

    val elemToMatch = "ZZZ"
    var current = "AAA"
    var steps = 0

    while (current != elemToMatch) {
        for (i in lines[0].trim()) {
            val direction = i
            current = if (direction == 'R') {
                desertMap[current]!!.right
            } else {
                desertMap[current]!!.left
            }
            steps++

            if (current == elemToMatch) {
                break
            }
        }
    }

    println(steps)
}