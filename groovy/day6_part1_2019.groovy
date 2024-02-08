def orbits = [:]
new File("input.txt").eachLine { line ->
    def split = line.split("\\)")
    orbits[split[1]] = split[0]
}

def countOrbits = { obj ->
    def count = 0
    while (orbits[obj] != null) {
        obj = orbits[obj]
        count++
    }
    count
}

def totalOrbits = orbits.keySet().sum { countOrbits(it) }
println totalOrbits