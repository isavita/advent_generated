import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    var password = "abcdefgh"

    for (op in input) {
        password = applyOperation(op, password)
    }

    println(password)
}

fun applyOperation(op: String, password: String): String {
    val fields = op.split(" ")
    when (fields[0]) {
        "swap" -> {
            when (fields[1]) {
                "position" -> {
                    val x = fields[2][0].toInt() - '0'.toInt()
                    val y = fields[5][0].toInt() - '0'.toInt()
                    return swapPosition(password, x, y)
                }
                "letter" -> {
                    val x = fields[2][0]
                    val y = fields[5][0]
                    return swapLetter(password, x, y)
                }
                else -> return password
            }
        }
        "rotate" -> {
            when (fields[1]) {
                "left" -> {
                    val steps = fields[2][0] - '0'
                    return rotateLeft(password, steps)
                }
                "right" -> {
                    val steps = fields[2][0] - '0'
                    return rotateRight(password, steps)
                }
                "based" -> {
                    val x = fields[6][0]
                    return rotateBasedOnPosition(password, x)
                }
                else -> return password
            }
        }
        "reverse" -> {
            val x = fields[2][0] - '0'
            val y = fields[4][0] - '0'
            return reversePositions(password, x, y)
        }
        "move" -> {
            val x = fields[2][0] - '0'
            val y = fields[5][0] - '0'
            return movePosition(password, x, y)
        }
        else -> return password
    }
}

fun swapPosition(password: String, x: Int, y: Int): String {
    val chars = password.toCharArray()
    val temp = chars[x]
    chars[x] = chars[y]
    chars[y] = temp
    return String(chars)
}

fun swapLetter(password: String, x: Char, y: Char): String {
    return password.map { if (it == x) y else if (it == y) x else it }.joinToString("")
}

fun rotateLeft(password: String, steps: Int): String {
    val actualSteps = steps % password.length
    return password.substring(actualSteps) + password.substring(0, actualSteps)
}

fun rotateRight(password: String, steps: Int): String {
    val actualSteps = steps % password.length
    return password.takeLast(actualSteps) + password.dropLast(actualSteps)
}

fun rotateBasedOnPosition(password: String, x: Char): String {
    val index = password.indexOf(x)
    val steps = 1 + index + if (index >= 4) 1 else 0
    return rotateRight(password, steps)
}

fun reversePositions(password: String, x: Int, y: Int): String {
    val chars = password.toCharArray()
    for (i in x..(x + y) / 2) {
        val temp = chars[i]
        chars[i] = chars[x + y - i]
        chars[x + y - i] = temp
    }
    return String(chars)
}

fun movePosition(password: String, x: Int, y: Int): String {
    val chars = password.toCharArray().toMutableList()
    val char = chars.removeAt(x)
    chars.add(y, char)
    return chars.joinToString("")
}