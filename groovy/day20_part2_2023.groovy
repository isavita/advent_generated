
class Module {
    String name
    List<String> destinations
    String type
    boolean on = false // For flip-flop modules
    Map<String, String> memory = [:] // For conjunction modules

    Module(String name, List<String> destinations, String type = null) {
        this.name = name
        this.destinations = destinations
        this.type = type
    }

    String toString() {
        return "Module(name: $name, destinations: $destinations, type: $type, on: $on, memory: $memory)"
    }
}

def solvePart1(String input) {
    def modules = parseInput(input)

    long lowPulses = 0
    long highPulses = 0

    1000.times {
        def queue = [["button", "broadcaster", "low"]] // source, destination, pulseType
        lowPulses++ // Count initial button press

        while (queue) {
            def (source, destination, pulseType) = queue.remove(0)

            if (!modules.containsKey(destination)) {
                // Destination module doesn't exist (e.g., 'output' in the example)
                continue
            }

            def module = modules[destination]

            if (pulseType == "low") {
                lowPulses++
            } else {
                highPulses++
            }

            if (module.type == "%") {
                if (pulseType == "low") {
                    module.on = !module.on
                    def nextPulse = module.on ? "high" : "low"
                    module.destinations.each { dest ->
                        queue.add([module.name, dest, nextPulse])
                    }
                }
            } else if (module.type == "&") {
                module.memory[source] = pulseType
                def allHigh = module.memory.values().every { it == "high" }
                def nextPulse = allHigh ? "low" : "high"
                module.destinations.each { dest ->
                    queue.add([module.name, dest, nextPulse])
                }
            } else if (module.name == "broadcaster") {
                module.destinations.each { dest ->
                    queue.add([module.name, dest, pulseType])
                }
            }
        }
    }

    return lowPulses * highPulses
}

def solvePart2(String input) {
    def modules = parseInput(input)

    // Find the module that feeds into 'rx'
    def rxFeeder = modules.find { it.value.destinations.contains("rx") }?.key

    // Find the modules that feed into rxFeeder (conjunction module)
    def feederInputs = modules.findAll { it.value.destinations.contains(rxFeeder) }.collect { it.key }

    // Map to store when each input sends a high pulse
    def firstHighPulses = [:]
    feederInputs.each { firstHighPulses[it] = 0 }

    long buttonPresses = 0
    boolean rxLowFound = false

    while (!rxLowFound) {
        buttonPresses++
        def queue = [["button", "broadcaster", "low"]]

        while (queue) {
            def (source, destination, pulseType) = queue.remove(0)

            if (!modules.containsKey(destination)) {
                continue
            }

            def module = modules[destination]

            // Part 2 - Track when feederInputs send a high pulse to rxFeeder
            if (destination == rxFeeder && pulseType == "high" && feederInputs.contains(source)) {
                if (firstHighPulses[source] == 0) {
                    firstHighPulses[source] = buttonPresses
                }

                if (firstHighPulses.values().every { it > 0 }) {
                    // All inputs have sent a high pulse at least once
                    return firstHighPulses.values().inject(1) { acc, curr -> lcm(acc, curr) }
                }
            }


            if (destination == "rx" && pulseType == "low") {
                rxLowFound = true
                break // Exit the inner loop
            }


            if (module.type == "%") {
                if (pulseType == "low") {
                    module.on = !module.on
                    def nextPulse = module.on ? "high" : "low"
                    module.destinations.each { dest ->
                        queue.add([module.name, dest, nextPulse])
                    }
                }
            } else if (module.type == "&") {
                module.memory[source] = pulseType
                def allHigh = module.memory.values().every { it == "high" }
                def nextPulse = allHigh ? "low" : "high"
                module.destinations.each { dest ->
                    queue.add([module.name, dest, nextPulse])
                }
            } else if (module.name == "broadcaster") {
                module.destinations.each { dest ->
                    queue.add([module.name, dest, pulseType])
                }
            }
        }
    }

    return buttonPresses
}

def parseInput(String input) {
    def modules = [:]

    input.split("\n").each { line ->
        def parts = line.split(" -> ")
        def name = parts[0]
        def destinations = parts[1].split(", ").toList()

        String type = null
        if (name.startsWith("%") || name.startsWith("&")) {
            type = name[0]
            name = name.substring(1)
        }

        def module = new Module(name, destinations, type)
        modules[name] = module
    }

    // Initialize conjunction module memory
    modules.each { moduleName, module ->
        if (module.type == "&") {
            modules.each { otherModuleName, otherModule ->
                if (otherModule.destinations.contains(moduleName)) {
                    module.memory[otherModuleName] = "low"
                }
            }
        }
    }
    return modules
}

long gcd(long a, long b) {
    while (b != 0) {
        long temp = b
        b = a % b
        a = temp
    }
    return a
}

long lcm(long a, long b) {
    return (a * b) / gcd(a, b)
}

def inputFile = new File("input.txt")
def input = inputFile.text.trim()

println "Part 1: ${solvePart1(input)}"
println "Part 2: ${solvePart2(input)}"
