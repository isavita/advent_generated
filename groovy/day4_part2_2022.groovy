
def input = new File("input.txt").readLines()

def countFullyContained = 0
def countOverlap = 0

input.each { line ->
    def ranges = line.split(",")
    def firstRange = ranges[0].split("-").collect { it.toInteger() }
    def secondRange = ranges[1].split("-").collect { it.toInteger() }

    if ((firstRange[0] <= secondRange[0] && firstRange[1] >= secondRange[1]) ||
            (secondRange[0] <= firstRange[0] && secondRange[1] >= firstRange[1])) {
        countFullyContained++
    }

    if (firstRange[0] <= secondRange[0] && firstRange[1] >= secondRange[0] ||
            secondRange[0] <= firstRange[0] && secondRange[1] >= firstRange[0]) {
        countOverlap++
    }
}

println countFullyContained
println countOverlap
