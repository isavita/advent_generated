
def file = new File("input.txt")
def banks = file.text.tokenize().collect { it as Integer }

def seen = [:]
def cycles = 0

while (true) {
    def state = banks.toString()

    if (seen.containsKey(state)) {
        println("The size of the loop is ${cycles - seen[state]}")
        return
    }
    seen[state] = cycles

    def maxIndex = banks.indexOf(banks.max())

    def blocks = banks[maxIndex]
    banks[maxIndex] = 0
    (1..blocks).each { i ->
        banks[(maxIndex + i) % banks.size()]++
    }

    cycles++
}
