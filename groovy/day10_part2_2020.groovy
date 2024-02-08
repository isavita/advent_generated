
def file = new File("input.txt")
def adapters = [0]

file.eachLine { line ->
    adapters.add(line.toInteger())
}

adapters.sort()
adapters.add(adapters.last() + 3)

println countArrangements(adapters)

def countArrangements(adapters) {
    def ways = [:].withDefault{ 0L }
    ways[0] = 1

    for (i in 1..<adapters.size()) {
        def currentJoltage = adapters[i]
        [1, 2, 3].each { diff ->
            ways[currentJoltage] += ways[currentJoltage - diff]
        }
    }

    return ways[adapters.last()]
}
