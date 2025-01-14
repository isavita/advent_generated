
def parse(String input) {
    def parts = input.split("\n\n")
    if (parts.size() != 2) return null

    def gates = []
    parts[1].split("\n").each { line ->
        if (!line) return
        def parts2 = line.split(" -> ")
        if (parts2.size() != 2) return
        def gateParts = parts2[0].split(" ")
        if (gateParts.size() != 3) return
        gates << [gate: [a: gateParts[0], op: gateParts[1], b: gateParts[2]], output: parts2[1]]
    }
    return gates
}

def createLookups(gates) {
    def lookup = [:]
    def reverseLookup = [:]

    gates.each { g ->
        lookup[g.output] = g.gate
        def inputs = [g.gate.a, g.gate.b].sort()
        def key = "${inputs[0]}_${g.gate.op}_${inputs[1]}"
        reverseLookup[key] = g.output
    }
    return [lookup: lookup, reverseLookup: reverseLookup]
}

def swap(pairs, gates, a, b) {
    pairs << [a, b]
    gates.each { g ->
        if (g.output == a) {
            g.output = b
        } else if (g.output == b) {
            g.output = a
        }
    }
}

def getReverseLookupKey(a, op, b) {
    def inputs = [a, b].sort()
    return "${inputs[0]}_${op}_${inputs[1]}"
}

def solution(gates) {
    def pairs = []
    def numZ = gates.count { it.output.startsWith("z") }

    while (pairs.size() < 4) {
        def adder = ""
        def carry = ""
        def lookups = createLookups(gates)
        def lookup = lookups.lookup
        def reverseLookup = lookups.reverseLookup

        (0..<numZ).each { i ->
            def xi = "x${String.format('%02d', i)}"
            def yi = "y${String.format('%02d', i)}"
            def zi = "z${String.format('%02d', i)}"

            if (i == 0) {
                adder = reverseLookup[getReverseLookupKey(xi, "XOR", yi)]
                carry = reverseLookup[getReverseLookupKey(xi, "AND", yi)]
            } else {
                def bit = reverseLookup[getReverseLookupKey(xi, "XOR", yi)]
                if (bit) {
                    adder = reverseLookup[getReverseLookupKey(bit, "XOR", carry)]
                    if (adder) {
                        def c1 = reverseLookup[getReverseLookupKey(xi, "AND", yi)]
                        def c2 = reverseLookup[getReverseLookupKey(bit, "AND", carry)]
                        carry = reverseLookup[getReverseLookupKey(c1, "OR", c2)]
                    }
                }
            }

            if (!adder) {
                def gate = lookup[zi]
                def bitKey = getReverseLookupKey(xi, "XOR", yi)
                def bit = reverseLookup[bitKey]
                if (reverseLookup[getReverseLookupKey(gate.a, "XOR", carry)]) {
                    swap(pairs, gates, bit, gate.a)
                    return
                } else if (reverseLookup[getReverseLookupKey(gate.b, "XOR", carry)]) {
                    swap(pairs, gates, bit, gate.b)
                    return
                }
            } else if (adder != zi) {
                swap(pairs, gates, adder, zi)
                return
            }
        }
    }

    def result = pairs.flatten().sort()
    return result.join(",")
}

def main() {
    def inputFile = new File("input.txt")
    if (!inputFile.exists()) {
        println "Error: input.txt not found"
        return
    }
    def input = inputFile.text
    def gates = parse(input)
    if (!gates) {
        println "Error parsing input"
        return
    }
    println solution(gates)
}

main()
