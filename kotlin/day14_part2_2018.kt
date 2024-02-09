import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    val scoreboard = mutableListOf(3, 7)
    var elf1 = 0
    var elf2 = 1
    val inputLen = input.length
    val inputSequence = IntArray(inputLen)

    for (i in 0 until inputLen) {
        inputSequence[i] = input[i].toString().toInt()
    }

    while (true) {
        val newScore = scoreboard[elf1] + scoreboard[elf2]
        if (newScore >= 10) {
            scoreboard.add(newScore / 10)
            if (checkSequence(scoreboard, inputSequence)) {
                break
            }
        }
        scoreboard.add(newScore % 10)
        if (checkSequence(scoreboard, inputSequence)) {
            break
        }

        elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.size
        elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.size
    }

    println(scoreboard.size - inputLen)
}

fun checkSequence(scoreboard: List<Int>, sequence: IntArray): Boolean {
    if (scoreboard.size < sequence.size) {
        return false
    }
    val start = scoreboard.size - sequence.size
    for (i in sequence.indices) {
        if (scoreboard[start + i] != sequence[i]) {
            return false
        }
    }
    return true
}