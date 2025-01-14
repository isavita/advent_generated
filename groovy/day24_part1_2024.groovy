
def wires = [:]
def gates = []

def wireRegex = ~/^(\w+):\s*([01])$/
def gateRegex = ~/^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$/

def parsingWires = true
new File("input.txt").eachLine { line ->
    line = line.trim()
    if (line == "") {
        parsingWires = false
    } else if (parsingWires) {
        def matcher = wireRegex.matcher(line)
        if (matcher.matches()) {
            wires[matcher[0][1]] = matcher[0][2].toInteger()
        }
    } else {
        def matcher = gateRegex.matcher(line)
        if (matcher.matches()) {
            gates << [input1: matcher[0][1], operation: matcher[0][2], input2: matcher[0][3], output: matcher[0][4]]
        }
    }
}

while (!gates.isEmpty()) {
    def newGates = []
    gates.each { gate ->
        def val1 = wires[gate.input1]
        def val2 = wires[gate.input2]
        if (val1 != null && val2 != null) {
            def outputVal
            switch (gate.operation) {
                case "AND": outputVal = (val1 && val2) ? 1 : 0; break
                case "OR": outputVal = (val1 || val2) ? 1 : 0; break
                case "XOR": outputVal = (val1 != val2) ? 1 : 0; break
            }
            wires[gate.output] = outputVal
        } else {
            newGates << gate
        }
    }
    if (newGates.size() == gates.size()) {
        break
    }
    gates = newGates
}

def zWires = [:]
def zRegex = ~/^z(\d+)$/
wires.each { wire, val ->
    def matcher = zRegex.matcher(wire)
    if (matcher.matches()) {
        zWires[matcher[0][1].toInteger()] = val
    }
}

def indices = zWires.keySet().sort()
def binaryString = indices.reverse().collect { zWires[it] }.join()
def decimalValue = Long.parseLong(binaryString, 2)

println decimalValue
