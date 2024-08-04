import java.io.File
import kotlin.math.min

fun main() {
    val input = File("input.txt").readText().trim()
    val lines = input.split("\n")

    var ground = mutableListOf(mutableListOf('+'))
    var maxX = 0
    var minX = 0
    var maxY = 0
    var minY = 20
    val xOffset = 500
    val yOffset = 0

    for (line in lines) {
        val split = Regex("[=, .]+").split(line)
        if (split[0] == "x") {
            val x = split[1].toInt() - xOffset
            val y1 = split[3].toInt() - yOffset
            val y2 = split[4].toInt() - yOffset

            while (x >= maxX) {
                maxX++
                for (row in ground) {
                    row.add('.')
                }
            }
            while (x <= minX) {
                minX--
                for (row in ground) {
                    row.add(0, '.')
                }
            }
            while (y2 > maxY) {
                maxY++
                ground.add(MutableList(ground[0].size) { '.' })
            }
            minY = min(minY, y1)
            for (i in y1..y2) {
                ground[i][x - minX] = '#'
            }
        } else {
            val y = split[1].toInt() - yOffset
            val x1 = split[3].toInt() - xOffset
            val x2 = split[4].toInt() - xOffset

            while (y > maxY) {
                maxY++
                ground.add(MutableList(ground[0].size) { '.' })
            }
            while (x2 >= maxX) {
                maxX++
                for (row in ground) {
                    row.add('.')
                }
            }
            while (x1 <= minX) {
                minX--
                for (row in ground) {
                    row.add(0, '.')
                }
            }
            minY = min(minY, y)
            for (i in x1..x2) {
                ground[y][i - minX] = '#'
            }
        }
    }

    var waterCount = 0
    var flowCount = 0
    val roundLimit = 200000

    while (ground[1][-minX] != '|' && waterCount < roundLimit) {
        var canMove = true
        var x = -minX
        var y = 1
        var tryLeft = 0
        while (canMove) {
            if (y + 1 > maxY || ground[y + 1][x] == '|') {
                ground[y][x] = '|'
                canMove = false
                if (y >= minY) {
                    flowCount++
                }
            } else if (ground[y + 1][x] == '.') {
                y++
                tryLeft = 0
            } else if (ground[y + 1][x] == '#' || ground[y + 1][x] == '~') {
                if ((tryLeft == 1 && ground[y][x - 1] == '|') ||
                    (tryLeft == 2 && ground[y][x + 1] == '|') ||
                    (ground[y][x + 1] == '|' && ground[y][x - 1] != '.') ||
                    (ground[y][x + 1] != '.' && ground[y][x - 1] == '|')) {
                    ground[y][x] = '|'
                    flowCount++
                    canMove = false
                    var i = x + 1
                    while (i < ground[y].size && ground[y][i] == '~') {
                        ground[y][i] = '|'
                        waterCount--
                        flowCount++
                        i++
                    }
                    i = x - 1
                    while (i >= 0 && ground[y][i] == '~') {
                        ground[y][i] = '|'
                        waterCount--
                        flowCount++
                        i--
                    }
                } else if ((tryLeft == 0 && ground[y][x - 1] == '.') ||
                    (tryLeft == 1 && ground[y][x - 1] == '.')) {
                    x--
                    tryLeft = 1
                } else if ((tryLeft == 0 && ground[y][x + 1] == '.') ||
                    (tryLeft == 2 && ground[y][x + 1] == '.')) {
                    x++
                    tryLeft = 2
                } else {
                    canMove = false
                    ground[y][x] = '~'
                    waterCount++
                }
            }
        }
    }
    println(flowCount + waterCount)
}