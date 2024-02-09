import java.io.File

data class Module(
    var name: String,
    var prefix: Char,
    var destinations: List<String>,
    var state: Boolean,
    var memory: MutableMap<String, PulseValue>
)

data class Pulse(
    var value: PulseValue,
    var fromName: String,
    var toName: String
)

enum class PulseValue {
    Low, High
}

const val FlipFlop: Char = '%'
const val Conjunction: Char = '&'

fun parseInput(input: List<String>): Map<String, Module> {
    val prefixes = listOf(FlipFlop, Conjunction)
    val modules = mutableMapOf<String, Module>()

    for (line in input) {
        val parts = line.split(" -> ")

        val module = Module("", ' ', emptyList(), false, mutableMapOf())
        var isPrefix = false
        for (prefix in prefixes) {
            if (parts[0][0] == prefix) {
                module.prefix = prefix
                module.name = parts[0].substring(1)
                isPrefix = true
                continue
            }
        }
        if (!isPrefix) {
            module.name = parts[0]
        }
        module.destinations = parts[1].split(", ")
        module.memory = mutableMapOf()

        modules[module.name] = module
    }

    for (module in modules.values) {
        for (destName in module.destinations) {
            if (modules.containsKey(destName) && modules[destName]?.prefix == Conjunction) {
                modules[destName]?.memory?.put(module.name, PulseValue.Low)
            }
        }
    }

    return modules
}

fun pushButton(modules: Map<String, Module>, startPulse: Pulse, numCycle: Int): Pair<Int, Int> {
    var cntLow = 0
    var cntHigh = 0
    val pulseQueue = mutableListOf<Pulse>()

    repeat(numCycle) {
        pulseQueue.add(startPulse)

        while (pulseQueue.isNotEmpty()) {
            val pulse = pulseQueue.removeAt(0)

            if (pulse.value == PulseValue.Low) {
                cntLow++
            } else {
                cntHigh++
            }

            if (!modules.containsKey(pulse.toName)) {
                continue
            }

            val module = modules[pulse.toName]!!
            val newPulseValue: PulseValue
            when (module.prefix) {
                FlipFlop -> {
                    if (pulse.value == PulseValue.Low) {
                        module.state = !module.state
                        newPulseValue = if (module.state) PulseValue.High else PulseValue.Low
                    } else {
                        continue
                    }
                }
                Conjunction -> {
                    module.memory[pulse.fromName] = pulse.value
                    var isHighForAll = true
                    for (value in module.memory.values) {
                        if (value == PulseValue.Low) {
                            isHighForAll = false
                            break
                        }
                    }
                    newPulseValue = if (isHighForAll) PulseValue.Low else PulseValue.High
                }
                else -> newPulseValue = pulse.value
            }

            for (destName in module.destinations) {
                val newPulse = Pulse(newPulseValue, pulse.toName, destName)
                pulseQueue.add(newPulse)
            }
        }
    }

    return Pair(cntLow, cntHigh)
}

fun solve(input: List<String>): Int {
    val startPulse = Pulse(PulseValue.Low, "button", "broadcaster")
    val numCycle = 1000

    val modules = parseInput(input)

    val (cntLow, cntHigh) = pushButton(modules, startPulse, numCycle)

    return cntLow * cntHigh
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}