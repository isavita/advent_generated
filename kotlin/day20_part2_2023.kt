import java.io.File

data class FlipFlop(val name: String, val moduleType: Int, val connectsTo: List<String>, var state: Boolean)
data class Conjunction(val name: String, val moduleType: Int, val connectsTo: List<String>, val watches: MutableMap<String, Boolean>)
data class Broadcaster(val name: String, val moduleType: Int, val connectsTo: List<String>)

data class State(val from: String, val name: String, val pulse: Boolean)

const val BROADCASTER = 0
const val FLIP_FLOP = 1
const val CONJUNCTION = 2

fun handleLine(line: String, connections: MutableMap<String, Any>) {
    if ("broadcaster" in line) {
        val str = line.split(" -> ")
        val module = Broadcaster(str[0], BROADCASTER, str[1].split(", "))
        connections[module.name] = module
    } else if ("%" in line) {
        val str = line.split(" -> ")
        val module = FlipFlop(str[0].substring(1), FLIP_FLOP, str[1].split(", "), false)
        connections[module.name] = module
    } else {
        val str = line.split(" -> ")
        val module = Conjunction(str[0].substring(1), CONJUNCTION, str[1].split(", "), mutableMapOf())
        connections[module.name] = module
    }
}

fun completeWatches(connections: MutableMap<String, Any>) {
    for (module in connections.values) {
        if (module is Conjunction) {
            for (module2 in connections.values) {
                when (module2) {
                    is FlipFlop -> {
                        for (name in module2.connectsTo) {
                            if (name == module.name) {
                                module.watches[module2.name] = false
                            }
                        }
                    }
                    is Conjunction -> {
                        for (name in module2.connectsTo) {
                            if (name == module.name) {
                                module.watches[module2.name] = false
                            }
                        }
                    }
                }
            }
        }
    }
}

fun simulatePress(connections: MutableMap<String, Any>, loops: MutableMap<String, Int>, pressNumber: Int): Pair<IntArray, Boolean> {
    val queue = mutableListOf(State("button", "broadcaster", false))
    var pulses = intArrayOf(1, 0)
    var found = false

    while (queue.isNotEmpty()) {
        val currState = queue.removeAt(0)
        val module = connections[currState.name]

        if (currState.name == "out") continue
        if (currState.name == "rx" && !currState.pulse) found = true

        val pulse = currState.pulse
        when (module) {
            is Broadcaster -> {
                for (name in module.connectsTo) {
                    queue.add(State(module.name, name, pulse))
                    if (pulse) pulses[1]++ else pulses[0]++
                }
            }
            is FlipFlop -> {
                if (!pulse) {
                    module.state = !module.state
                    for (name in module.connectsTo) {
                        queue.add(State(module.name, name, module.state))
                        if (module.state) pulses[1]++ else pulses[0]++
                    }
                }
            }
            is Conjunction -> {
                module.watches[currState.from] = pulse
                val allTrue = module.watches.values.all { it }
                for (name in module.connectsTo) {
                    queue.add(State(module.name, name, !allTrue))
                    if (!allTrue) pulses[1]++ else pulses[0]++
                }
                if (loops.containsKey(currState.name) && !allTrue && loops[currState.name] == -1) {
                    loops[currState.name] = pressNumber
                }
            }
        }
    }
    return pulses to found
}

fun connectsTo(from: String, to: String, connections: Map<String, Any>): Boolean {
    when (val module = connections[from]) {
        is Broadcaster -> return module.connectsTo.contains(to)
        is FlipFlop -> return module.connectsTo.contains(to)
        is Conjunction -> return module.connectsTo.contains(to)
    }
    return false
}

fun main() {
    val lines = File("input.txt").readLines()
    val connections = mutableMapOf<String, Any>()

    for (line in lines) handleLine(line, connections)
    completeWatches(connections)

    val pxPrev = connections.keys.filter { connectsTo(it, "rx", connections) }
    if (pxPrev.size != 1) throw Exception("Error: more than one pxPrev")

    val conj = connections[pxPrev[0]] as Conjunction
    val loopLengths = conj.watches.keys.associateWith { -1 }.toMutableMap()

    var pressNumber = 0
    while (true) {
        pressNumber++
        val (_, found) = simulatePress(connections, loopLengths, pressNumber)
        if (found) break
        if (loopLengths.values.all { it != -1 }) break
    }

    var sum = 1L
    for (length in loopLengths.values) {
        sum *= length
    }

    println(sum)
}