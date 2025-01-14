
def parseInput(input) {
    def modules = [:]
    input.each { line ->
        def parts = line.split(' -> ')
        def module = [:]
        def isPrefix = false
        ['%', '&'].each { prefix ->
            if (parts[0][0] == prefix) {
                module.prefix = prefix
                module.name = parts[0][1..-1]
                isPrefix = true
            }
        }
        if (!isPrefix) {
            module.name = parts[0]
        }
        module.destinations = parts[1].split(', ')
        module.memory = [:]
        modules[module.name] = module
    }
    modules.each { name, module ->
        module.destinations.each { destName ->
            if (modules[destName] && modules[destName].prefix == '&') {
                modules[destName].memory[name] = 0
            }
        }
    }
    modules
}

def pushButton(modules, startPulse, numCycle) {
    def cntLow = 0
    def cntHigh = 0
    def pulseQueue = []

    numCycle.times {
        pulseQueue << startPulse

        while (!pulseQueue.isEmpty()) {
            def pulse = pulseQueue.remove(0)

            if (pulse.value == 0) {
                cntLow++
            } else {
                cntHigh++
            }

            if (!modules[pulse.toName]) {
                continue
            }

            def module = modules[pulse.toName]
            def newPulseValue
            switch (module.prefix) {
                case '%':
                    if (pulse.value == 0) {
                        module.state = !module.state
                        newPulseValue = module.state ? 1 : 0
                    } else {
                        continue
                    }
                    break
                case '&':
                    module.memory[pulse.fromName] = pulse.value
                    def isHighForAll = module.memory.values().every { it == 1 }
                    newPulseValue = isHighForAll ? 0 : 1
                    break
                default:
                    newPulseValue = pulse.value
            }

            module.destinations.each { destName ->
                pulseQueue << [value: newPulseValue, fromName: pulse.toName, toName: destName]
            }
        }
    }

    [cntLow, cntHigh]
}

def solve(input) {
    def startPulse = [value: 0, fromName: 'button', toName: 'broadcaster']
    def numCycle = 1000

    def modules = parseInput(input)

    def (cntLow, cntHigh) = pushButton(modules, startPulse, numCycle)

    cntLow * cntHigh
}

def input = new File('input.txt').readLines()
println solve(input)
