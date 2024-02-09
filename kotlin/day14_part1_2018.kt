import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().toInt()

    val scoreboard = mutableListOf(3, 7)
    var elf1 = 0
    var elf2 = 1

    while (scoreboard.size < input + 10) {
        val newScore = scoreboard[elf1] + scoreboard[elf2]
        if (newScore >= 10) {
            scoreboard.add(newScore / 10)
        }
        scoreboard.add(newScore % 10)

        elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.size
        elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.size
    }

    for (i in input until input + 10) {
        print(scoreboard[i])
    }
    println()
}