
def main() {
    def inputStr = new File("input.txt").text.trim()
    def lines = inputStr.split("\n")

    def ground = [['+']]
    def maxX = 0, minX = 0, maxY = 0, minY = 20
    def xOffset = 500, yOffset = 0

    lines.each { line ->
        def split = line.split(/[=, .]+/)
        if (split[0] == "x") {
            def x = split[1].toInteger() - xOffset
            def y1 = split[3].toInteger() - yOffset
            def y2 = split[4].toInteger() - yOffset

            while (x >= maxX) {
                maxX++
                ground.each { it << '.' }
            }
            while (x <= minX) {
                minX--
                ground.each { it.add(0, '.') }
            }
            while (y2 > maxY) {
                maxY++
                ground << (['.'] * ground[0].size())
            }
            if (y1 < minY) {
                minY = y1
            }
            (y1..y2).each { i ->
                ground[i][x - minX] = '#'
            }

        } else {
            def y = split[1].toInteger() - yOffset
            def x1 = split[3].toInteger() - xOffset
            def x2 = split[4].toInteger() - xOffset

            while (y > maxY) {
                maxY++
                ground << (['.'] * ground[0].size())
            }
            while (x2 >= maxX) {
                maxX++
                ground.each { it << '.' }
            }
            while (x1 <= minX) {
                minX--
                ground.each { it.add(0, '.') }
            }
            (x1..x2).each { i ->
                ground[y][i - minX] = '#'
            }
            if (y < minY) {
                minY = y
            }
        }
    }

    def waterCount = 0
    def flowCount = 0
    def roundLimit = 200000

    while (ground[1][-minX] != '|' && waterCount < roundLimit) {
        def canMove = true
        def x = -minX
        def y = 1
        def tryLeft = 0
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
            } else if (ground[y + 1][x] in ['#', '~']) {
                if (((tryLeft == 1 && ground[y][x - 1] == '|') ||
                     (tryLeft == 2 && ground[y][x + 1] == '|') ||
                     (ground[y][x + 1] == '|' && ground[y][x - 1] != '.') ||
                     (ground[y][x + 1] != '.' && ground[y][x - 1] == '|'))) {
                    ground[y][x] = '|'
                    flowCount++
                    canMove = false
                    (x + 1..<ground[0].size()).each { i ->
                        if (ground[y][i] == '~') {
                            ground[y][i] = '|'
                            waterCount--
                            flowCount++
                        } else {
                            return
                        }
                    }
                    (x - 1..0).reverseEach { i ->
                        if (ground[y][i] == '~') {
                            ground[y][i] = '|'
                            waterCount--
                            flowCount++
                        } else {
                            return
                        }
                    }
                } else if (((tryLeft == 0 && ground[y][x - 1] == '.') ||
                            (tryLeft == 1 && ground[y][x - 1] == '.'))) {
                    x--
                    tryLeft = 1
                } else if (((tryLeft == 0 && ground[y][x + 1] == '.') ||
                            (tryLeft == 2 && ground[y][x + 1] == '.'))) {
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

main()
