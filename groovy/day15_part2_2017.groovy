
def inputFile = new File("input.txt")
def lines = []
inputFile.eachLine { line -> lines.add(line) }

def genAStart = lines[0].toLong()
def genBStart = lines[1].toLong()

def genAFactor = 16807 as Long
def genBFactor = 48271 as Long
def modulus = 2147483647 as Long

def genA = genAStart
def genB = genBStart
def matches = 0

(0..4999999).each {
    // Generate next value for A that is a multiple of 4
    while (true) {
        genA = (genA * genAFactor) % modulus
        if (genA % 4 == 0) {
            break
        }
    }

    // Generate next value for B that is a multiple of 8
    while (true) {
        genB = (genB * genBFactor) % modulus
        if (genB % 8 == 0) {
            break
        }
    }

    if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
        matches++
    }
}

println matches
