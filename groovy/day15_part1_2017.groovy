
def file = new File("input.txt")
def lines = file.readLines()
def genAStart = lines[0] as Long
def genBStart = lines[1] as Long

def genAFactor = 16807 as Long
def genBFactor = 48271 as Long
def modulus = 2147483647 as Long

def genA = genAStart
def genB = genBStart
def matches = 0

for (int i = 0; i < 40000000; i++) {
    genA = (genA * genAFactor) % modulus
    genB = (genB * genBFactor) % modulus

    if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
        matches++
    }
}

println matches
