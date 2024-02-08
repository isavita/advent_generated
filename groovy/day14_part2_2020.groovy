
def lines = new File("input.txt").readLines()

def applyMask(value, mask) {
    def result = value
    mask.eachWithIndex { bit, index ->
        if (bit == '1') {
            result = result | (1L << (35 - index))
        } else if (bit == 'X') {
            result = result & ~(1L << (35 - index))
        }
    }
    return result
}

def applyAddressMask(address, mask) {
    def addresses = [address]
    mask.eachWithIndex { bit, index ->
        if (bit == '1') {
            addresses = addresses.collect { it | (1L << (35 - index)) }
        } else if (bit == 'X') {
            addresses = addresses.collect { addr -> [addr | (1L << (35 - index)), addr & ~(1L << (35 - index))] }.flatten()
        }
    }
    return addresses
}

def memory = [:]
def mask = ""
lines.each { line ->
    def parts = line.split(" = ")
    if (parts[0] == "mask") {
        mask = parts[1]
    } else {
        def address = parts[0].replaceAll("[^0-9]", "").toLong()
        def value = parts[1].toLong()
        def maskedValue = applyMask(value, mask)
        memory[address] = maskedValue
    }
}

def memorySum = memory.values().sum()
println memorySum

def memory2 = [:]
mask = ""
lines.each { line ->
    def parts = line.split(" = ")
    if (parts[0] == "mask") {
        mask = parts[1]
    } else {
        def address = parts[0].replaceAll("[^0-9]", "").toLong()
        def value = parts[1].toLong()
        def addresses = applyAddressMask(address, mask)
        addresses.each { addr -> memory2[addr] = value }
    }
}

def memorySum2 = memory2.values().sum()
println memorySum2
