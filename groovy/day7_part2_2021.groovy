
def input = new File("input.txt").text.split(",").collect { it.toInteger() }

def calculateFuel = { position, crabs ->
    crabs.collect { Math.abs(it - position) * (Math.abs(it - position) + 1) / 2 }.sum()
}

def minFuel = Integer.MAX_VALUE
def bestPosition = 0

for (i in 0..input.max()) {
    def fuel = calculateFuel(i, input)
    if (fuel < minFuel) {
        minFuel = fuel
        bestPosition = i
    }
}

println minFuel
