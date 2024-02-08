
def adapters = new File("input.txt").readLines().collect { it.toInteger() }.sort()

def differences = [0, 0, 1]
def current = 0

adapters.each { adapter ->
    differences[adapter - current - 1]++
    current = adapter
}

println differences[0] * differences[2]
