
def input = new File("input.txt").readLines()

def gammaRate = ""
def epsilonRate = ""

(0..input[0].size() - 1).each { i ->
    def countMap = input.collect { it[i] }.countBy { it }
    gammaRate += countMap.max { a, b -> a.value <=> b.value }.key
    epsilonRate += countMap.min { a, b -> a.value <=> b.value }.key
}

def powerConsumption = Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2)
println powerConsumption
