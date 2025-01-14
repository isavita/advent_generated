
import java.nio.file.Files
import java.nio.file.Paths

def input = new String(Files.readAllBytes(Paths.get("input.txt")))
def lines = input.trim().split('\n')

def ground = [['+']]
def maxX = 0
def minX = 0
def maxY = 0
def minY = 20
def xOffset = 500
def yOffset = 0

lines.each { line ->
    def split = line.split(/[=, .]+/)
    if (split[0] == 'x') {
        def x = split[1].toInteger() - xOffset
        def y1 = split[3].toInteger() - yOffset
        def y2 = split[4].toInteger() - yOffset

        while (x > maxX) {
            maxX++
            ground.each { it.add('.') }
        }
        while (x < minX) {
            minX--
            ground.each { it.add(0, '.') }
        }
        while (y2 > maxY) {
            maxY++
            ground.add(['.'] * ground[0].size())
        }
        if (y1 < minY) {
            minY = y1
        }
        (y1..y2).each { y ->
            ground[y][x - minX] = '#'
        }
    } else {
        def y = split[1].toInteger() - yOffset
        def x1 = split[3].toInteger() - xOffset
        def x2 = split[4].toInteger() - xOffset

        while (y > maxY) {
            maxY++
            ground.add(['.'] * ground[0].size())
        }
        while (x2 > maxX) {
            maxX++
            ground.each { it.add('.') }
        }
        while (x1 < minX) {
            minX--
            ground.each { it.add(0, '.') }
        }
        (x1..x2).each { x ->
            ground[y][x - minX] = '#'
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
        } else if (ground[y + 1][x] == '#' || ground[y + 1][x] == '~') {
            if ((tryLeft == 1 && ground[y][x - 1] == '|') ||
                (tryLeft == 2 && ground[y][x + 1] == '|') ||
                (ground[y][x + 1] == '|' && ground[y][x - 1] != '.') ||
                (ground[y][x + 1] != '.' && ground[y][x - 1] == '|')) {
                ground[y][x] = '|'
                flowCount++
                canMove = false
                for (i = x + 1; ground[y][i] == '~'; i++) {
                    ground[y][i] = '|'
                    waterCount--
                    flowCount++
                }
                for (i = x - 1; ground[y][i] == '~'; i--) {
                    ground[y][i] = '|'
                    waterCount--
                    flowCount++
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

println waterCount
