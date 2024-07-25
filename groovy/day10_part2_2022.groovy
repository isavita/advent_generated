
def processInstructions(fileName) {
    def instructions = new File(fileName).readLines()
    def x = 1
    def cycle = 0
    def signalStrengths = []
    def crt = []

    // Initialize CRT
    for (int i = 0; i < 6; i++) {
        crt << ['.'] * 40
    }

    instructions.each { instruction ->
        def parts = instruction.split(" ")
        def cmd = parts[0]
        def value = parts.size() > 1 ? parts[1].toInteger() : 0

        if (cmd == "noop") {
            drawPixel(cycle, x, crt)
            cycle++
        } else if (cmd == "addx") {
            drawPixel(cycle, x, crt)
            cycle++
            drawPixel(cycle, x, crt)
            cycle++
            x += value
        }

        // Check for signal strengths at specific cycles
        if ((cycle - 20) % 40 == 0 && cycle <= 220) {
            signalStrengths << (cycle * x)
        }
    }

    // Print the sum of signal strengths
    println "Sum of signal strengths: ${signalStrengths.sum()}"

    // Print the CRT output
    crt.each { row ->
        println row.join('')
    }
}

def drawPixel(cycle, x, crt) {
    int row = cycle / 40
    int col = cycle % 40
    if (col >= x - 1 && col <= x + 1) {
        crt[row][col] = '#'
    }
}

// Run the program
processInstructions('input.txt')
