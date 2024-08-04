import java.io.File

fun main() {
    val input = File("input.txt").readText().trim()
    val lines = input.split("\n")

    val ground = mutableListOf(mutableListOf('+'))
    var maxX = 0
    var minX = 0
    var maxY = 0
    var minY = 20
    val xOffset = 500
    val yOffset = 0

    for (line in lines) {
        val split = line.split("[=, .]+".toRegex())
        if (split[0] == "x") {
            val x = split[1].toInt() - xOffset
            val y1 = split[3].toInt() - yOffset
            val y2 = split[4].toInt() - yOffset

            while (x >= maxX) {
                maxX++
                for (j in ground.indices) {
                    ground[j].add('.')
                }
            }
            while (x <= minX) {
                minX--
                for (j in ground.indices) {
                    ground[j].add(0, '.')
                }
            }
            while (y2 > maxY) {
                maxY++
                ground.add(mutableListOf('.'))
                repeat(ground[0].size) { ground[ground.size - 1].add('.') }
            }
            if (y1 < minY) {
                minY = y1
            }
            for (i in y1..y2) {
                ground[i][x - minX] = '#'
            }
        } else {
            val y = split[1].toInt() - yOffset
            val x1 = split[3].toInt() - xOffset
            val x2 = split[4].toInt() - xOffset

            while (y > maxY) {
                maxY++
                ground.add(mutableListOf('.'))
                repeat(ground[0].size) { ground[ground.size - 1].add('.') }
            }
            while (x2 >= maxX) {
                maxX++
                for (j in ground.indices) {
                    ground[j].add('.')
                }
            }
            while (x1 <= minX) {
                minX--
                for (j in ground.indices) {
                    ground[j].add(0, '.')
                }
            }
            for (i in x1..x2) {
                ground[y][i - minX] = '#'
            }
            if (y < minY) {
                minY = y
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
                    (ground[y][x + 1] != '.' && ground[y][x - 1] == '|')
                ) {
                    ground[y][x] = '|'
                    flowCount++
                    canMove = false
                    for (i in x + 1 until ground[0].size) {
                        if (ground[y][i] == '~') {
                            ground[y][i] = '|'
                            waterCount--
                            flowCount++
                        } else {
                            break
                        }
                    }
                    for (i in x - 1 downTo 0) {
                        if (ground[y][i] == '~') {
                            ground[y][i] = '|'
                            waterCount--
                            flowCount++
                        } else {
                            break
                        }
                    }
                } else if ((tryLeft == 0 && ground[y][x - 1] == '.') ||
                    (tryLeft == 1 && ground[y][x - 1] == '.')
                ) {
                    x--
                    tryLeft = 1
                } else if ((tryLeft == 0 && ground[y][x + 1] == '.') ||
                    (tryLeft == 2 && ground[y][x + 1] == '.')
                ) {
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
    println(waterCount)
}