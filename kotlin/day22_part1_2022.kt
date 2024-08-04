import java.io.File

fun main() {
    val input = File("input.txt").readText()
    val parts = input.split("\n\n")
    val map = parts[0].split("\n")
    val path = parts[1]

    val instructions = parsePath(path)
    val (finalRow, finalCol, facing) = simulateMovement(map, instructions)

    val password = 1000 * finalRow + 4 * finalCol + facing
    println(password)
}

fun parsePath(path: String): List<String> {
    val instructions = mutableListOf<String>()
    var temp = ""
    for (char in path) {
        if (char.isDigit()) {
            temp += char
        } else {
            if (temp.isNotEmpty()) {
                instructions.add(temp)
                temp = ""
            }
            instructions.add(char.toString())
        }
    }
    if (temp.isNotEmpty()) {
        instructions.add(temp)
    }
    return instructions
}

fun simulateMovement(map: List<String>, instructions: List<String>): Triple<Int, Int, Int> {
    var row = 0
    var col = map[0].indexOfFirst { it == '.' }
    var facing = 0 // 0: right, 1: down, 2: left, 3: up

    for (instruction in instructions) {
        if (instruction.isDigit()) {
            val steps = instruction.toInt()
            for (step in 0 until steps) {
                val (newRow, newCol) = getNextPosition(map, row, col, facing)
                if (map[newRow][newCol] == '#') {
                    break
                }
                row = newRow
                col = newCol
            }
        } else {
            facing = when (instruction) {
                "R" -> (facing + 1) % 4
                "L" -> (facing + 3) % 4
                else -> facing
            }
        }
    }

    return Triple(row + 1, col + 1, facing)
}

fun getNextPosition(map: List<String>, row: Int, col: Int, facing: Int): Pair<Int, Int> {
    val directions = arrayOf(
        Pair(0, 1), // right
        Pair(1, 0), // down
        Pair(0, -1), // left
        Pair(-1, 0) // up
    )
    val (dr, dc) = directions[facing]
    var newRow = row + dr
    var newCol = col + dc

    if (newRow < 0 || newRow >= map.size || newCol < 0 || newCol >= map[newRow].length || map[newRow][newCol] == ' ') {
        // Wrap around
        when (facing) {
            0 -> { // right
                newCol = map[row].indexOfFirst { it != ' ' }
            }
            1 -> { // down
                newRow = map.indexOfFirst { it.length > col && it[col] != ' ' }
            }
            2 -> { // left
                newCol = map[row].indexOfLast { it != ' ' }
            }
            3 -> { // up
                newRow = map.indexOfLast { it.length > col && it[col] != ' ' }
            }
        }
    }

    return Pair(newRow, newCol)
}

fun String.isDigit() = this.all { it.isDigit() }