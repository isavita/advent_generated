import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    val screenWidth = 50
    val screenHeight = 6
    val screen = Array(screenHeight) { BooleanArray(screenWidth) }

    input.forEach { processInstruction(it, screen) }

    displayScreen(screen)
}

fun displayScreen(screen: Array<BooleanArray>) {
    screen.forEach { row ->
        row.forEach { pixel ->
            if (pixel) {
                print("#")
            } else {
                print(".")
            }
        }
        println()
    }
}

fun processInstruction(instruction: String, screen: Array<BooleanArray>) {
    val rectRegex = Regex("""rect (\d+)x(\d+)""")
    val rotateRowRegex = Regex("""rotate row y=(\d+) by (\d+)""")
    val rotateColumnRegex = Regex("""rotate column x=(\d+) by (\d+)""")

    when {
        rectRegex.matches(instruction) -> {
            val (a, b) = rectRegex.find(instruction)!!.destructured
            rect(screen, a.toInt(), b.toInt())
        }
        rotateRowRegex.matches(instruction) -> {
            val (a, b) = rotateRowRegex.find(instruction)!!.destructured
            rotateRow(screen, a.toInt(), b.toInt())
        }
        rotateColumnRegex.matches(instruction) -> {
            val (a, b) = rotateColumnRegex.find(instruction)!!.destructured
            rotateColumn(screen, a.toInt(), b.toInt())
        }
    }
}

fun rect(screen: Array<BooleanArray>, a: Int, b: Int) {
    for (y in 0 until b) {
        for (x in 0 until a) {
            screen[y][x] = true
        }
    }
}

fun rotateRow(screen: Array<BooleanArray>, row: Int, shift: Int) {
    val temp = BooleanArray(screen[0].size)
    for (i in screen[row].indices) {
        temp[(i + shift) % screen[0].size] = screen[row][i]
    }
    screen[row] = temp
}

fun rotateColumn(screen: Array<BooleanArray>, col: Int, shift: Int) {
    val temp = BooleanArray(screen.size)
    for (i in screen.indices) {
        temp[(i + shift) % screen.size] = screen[i][col]
    }
    for (i in screen.indices) {
        screen[i][col] = temp[i]
    }
}