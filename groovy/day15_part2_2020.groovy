
def startingNumbers = new File("input.txt").text.trim().split(',')

def spoken = [:]
def lastSpoken = 0

startingNumbers.eachWithIndex { number, i ->
    if (i == startingNumbers.size() - 1) {
        lastSpoken = number as int
    } else {
        spoken[number as int] = i + 1
    }
}

( startingNumbers.size() + 1 .. 30000000 ).each { turn ->
    def nextNumber = spoken.containsKey(lastSpoken) ? turn - 1 - spoken[lastSpoken] : 0
    spoken[lastSpoken] = turn - 1
    lastSpoken = nextNumber
}

println lastSpoken
