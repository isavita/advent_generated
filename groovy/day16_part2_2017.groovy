
def programs = "abcdefghijklmnop".toList()
def moves = new File("input.txt").text.trim().split(",")

def dance(programs, moves) {
    def currentPrograms = programs.clone()
    moves.each { move ->
        if (move.startsWith("s")) {
            def spinSize = move[1..-1].toInteger()
            currentPrograms = currentPrograms[-spinSize..-1] + currentPrograms[0..-spinSize - 1]
        } else if (move.startsWith("x")) {
            def parts = move[1..-1].split("/").collect { it.toInteger() }
            def temp = currentPrograms[parts[0]]
            currentPrograms[parts[0]] = currentPrograms[parts[1]]
            currentPrograms[parts[1]] = temp
        } else if (move.startsWith("p")) {
            def parts = move[1..-1].split("/")
            def indexA = currentPrograms.indexOf(parts[0])
            def indexB = currentPrograms.indexOf(parts[1])
            def temp = currentPrograms[indexA]
            currentPrograms[indexA] = currentPrograms[indexB]
            currentPrograms[indexB] = temp
        }
    }
    return currentPrograms
}

def part1Result = dance(programs, moves).join()
println "Part 1: ${part1Result}"

def seen = [:]
def currentPrograms = programs.clone()
def billion = 1000000000
def cycleStart = -1
def cycleLength = -1

for (int i = 0; i < billion; i++) {
    def state = currentPrograms.join()
    if (seen.containsKey(state)) {
        cycleStart = seen[state]
        cycleLength = i - cycleStart
        break
    }
    seen[state] = i
    currentPrograms = dance(currentPrograms, moves)
}

if (cycleStart != -1) {
    def remaining = (billion - cycleStart) % cycleLength
    currentPrograms = programs.clone()
    for (int i = 0; i < cycleStart + remaining; i++) {
        currentPrograms = dance(currentPrograms, moves)
    }
}

def part2Result = currentPrograms.join()
println "Part 2: ${part2Result}"
