import java.io.File

class Scrambler(private var pw: String) {
    fun swapPositions(x: Int, y: Int) {
        val chars = pw.toCharArray()
        chars[x] = pw[y]
        chars[y] = pw[x]
        pw = String(chars)
    }

    fun swapLetters(x: Char, y: Char) {
        swapPositions(pw.indexOf(x), pw.indexOf(y))
    }

    fun rotate(steps: Int) {
        val length = pw.length
        val actualSteps = (steps % length + length) % length
        pw = pw.takeLast(actualSteps) + pw.dropLast(actualSteps)
    }

    fun rotateLetter(x: Char) {
        val index = pw.indexOf(x)
        val rot = if (index >= 4) index + 2 else index + 1
        rotate(rot)
    }

    fun derotateLetter(x: Char) {
        val index = pw.indexOf(x)
        val rot = when {
            index % 2 == 1 -> -(index + 1) / 2
            index != 0 -> (6 - index) / 2
            else -> -1
        }
        rotate(rot)
    }

    fun reverse(x: Int, y: Int) {
        val chars = pw.toCharArray()
        var i = x
        var j = y
        while (i < j) {
            val temp = chars[i]
            chars[i] = chars[j]
            chars[j] = temp
            i++
            j--
        }
        pw = String(chars)
    }

    fun move(x: Int, y: Int) {
        val chars = pw.toCharArray()
        val ch = chars[x]
        if (x < y) {
            System.arraycopy(chars, x + 1, chars, x, y - x)
        } else {
            System.arraycopy(chars, y, chars, y + 1, x - y)
        }
        chars[y] = ch
        pw = String(chars)
    }

    fun scramble(instructions: List<String>, direction: Int): Scrambler {
        val revInstructions = if (direction < 0) instructions.reversed() else instructions
        for (instruction in revInstructions) {
            val line = instruction.split(" ")
            when {
                instruction.startsWith("swap") -> {
                    val x = line[2]
                    val y = line.last()
                    if (line[1] == "position") {
                        swapPositions(x.toInt(), y.toInt())
                    } else {
                        swapLetters(x[0], y[0])
                    }
                }
                instruction.startsWith("rotate") -> {
                    if (line[1] == "based") {
                        if (direction > 0) {
                            rotateLetter(line.last()[0])
                        } else {
                            derotateLetter(line.last()[0])
                        }
                    } else {
                        var x = line[2].toInt()
                        if (line[1] == "left") {
                            x = -x
                        }
                        if (direction < 0) {
                            x = -x
                        }
                        rotate(x)
                    }
                }
                instruction.startsWith("reverse") -> {
                    val x = line[2].toInt()
                    val y = line.last().toInt()
                    reverse(x, y)
                }
                instruction.startsWith("move") -> {
                    var x = line[2].toInt()
                    var y = line.last().toInt()
                    if (direction < 0) {
                        val temp = x
                        x = y
                        y = temp
                    }
                    move(x, y)
                }
            }
        }
        return this
    }

    fun unscramble(instructions: List<String>): Scrambler {
        return scramble(instructions, -1)
    }

    override fun toString(): String {
        return pw
    }
}

fun main() {
    val instructions = File("input.txt").readLines()
    val hashed = "fbgdceah"
    val scrambler = Scrambler(hashed)
    val result = scrambler.unscramble(instructions)
    println(result)
}