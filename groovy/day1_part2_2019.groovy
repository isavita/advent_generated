def processLine(line) {
    n = line.trim().toInteger()
    return n
}

def calcFuelMass(mass) {
    fuel = (Math.floor(mass / 3) - 2)
    if (fuel <= 0) {
        return 0
    }
    return fuel + calcFuelMass(fuel)
}

def getTotal(masses) {
    total = 0
    for (mass in masses) {
        total += calcFuelMass(mass)
    }
    return total
}

def file = new File("input.txt")
def reader = file.newReader()

def masses = []
while ((line = reader.readLine()) != null) {
    n = processLine(line)
    masses.add(n)
}
reader.close()

total = getTotal(masses)
println total