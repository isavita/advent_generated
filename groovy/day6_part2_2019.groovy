def orbits = [:]
def totalOrbits = 0
def YOUorbits = []
def SANorbits = []

new File('input.txt').eachLine { line ->
    def (center, orbiter) = line.tokenize(')')
    orbits[orbiter] = center
}

orbits.each { orbiter, center ->
    def current = orbiter
    while (orbits.containsKey(current)) {
        totalOrbits++
        current = orbits[current]
    }
}

def current = orbits['YOU']
while (orbits.containsKey(current)) {
    YOUorbits.add(current)
    current = orbits[current]
}

current = orbits['SAN']
while (orbits.containsKey(current)) {
    SANorbits.add(current)
    current = orbits[current]
}

def commonOrbit = YOUorbits.find { it in SANorbits }
def transfers = YOUorbits.indexOf(commonOrbit) + SANorbits.indexOf(commonOrbit)

println totalOrbits
println transfers