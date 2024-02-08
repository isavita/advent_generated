
def file = new File("input.txt")
def screen = new boolean[6][50]

file.eachLine { line ->
    processInstruction(line, screen)
}

displayScreen(screen)

def displayScreen(screen) {
    screen.each { row ->
        row.each { pixel ->
            if (pixel) {
                print("#")
            } else {
                print(".")
            }
        }
        println()
    }
}

def processInstruction(instruction, screen) {
    def rectRegex = ~/rect (\d+)x(\d+)/
    def rotateRowRegex = ~/rotate row y=(\d+) by (\d+)/
    def rotateColumnRegex = ~/rotate column x=(\d+) by (\d+)/

    if (instruction ==~ rectRegex) {
        def (a, b) = instruction.findAll(/\d+/)*.toInteger()
        rect(screen, a, b)
    } else if (instruction ==~ rotateRowRegex) {
        def (a, b) = instruction.findAll(/\d+/)*.toInteger()
        rotateRow(screen, a, b)
    } else if (instruction ==~ rotateColumnRegex) {
        def (a, b) = instruction.findAll(/\d+/)*.toInteger()
        rotateColumn(screen, a, b)
    }
}

def rect(screen, a, b) {
    (0..<b).each { y ->
        (0..<a).each { x ->
            screen[y][x] = true
        }
    }
}

def rotateRow(screen, row, shift) {
    def temp = new boolean[50]
    (0..49).each { i ->
        temp[(i + shift) % 50] = screen[row][i]
    }
    screen[row] = temp
}

def rotateColumn(screen, col, shift) {
    def temp = new boolean[6]
    (0..5).each { i ->
        temp[(i + shift) % 6] = screen[i][col]
    }
    (0..5).each { i ->
        screen[i][col] = temp[i]
    }
}
