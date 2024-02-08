def input = new File("input.txt").text.tokenize(',').collect { it.toInteger() }

def totalFuel = 0
def minFuel = Integer.MAX_VALUE
def alignPosition = 0

for (i in 0..input.max()) {
    totalFuel = input.collect { Math.abs(it - i) }.sum()
    
    if (totalFuel < minFuel) {
        minFuel = totalFuel
        alignPosition = i
    }
}

println alignPosition
println minFuel