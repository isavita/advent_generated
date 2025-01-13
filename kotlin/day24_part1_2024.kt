
import java.io.File

data class Gate(val input1: String, val input2: String, val operation: String, val output: String)

fun main() {
    val lines = File("input.txt").readLines()
    val wires = mutableMapOf<String, Int>()
    val gates = mutableListOf<Gate>()
    var parsingWires = true

    for (line in lines) {
        val trimmedLine = line.trim()
        if (trimmedLine.isEmpty()) {
            parsingWires = false
            continue
        }
        if (parsingWires) {
            val parts = trimmedLine.split(":")
            wires[parts[0].trim()] = parts[1].trim().toInt()
        } else {
            val parts = trimmedLine.split(" ")
            gates.add(Gate(parts[0], parts[2], parts[1], parts[4]))
        }
    }

    val remainingGates = gates.toMutableList()
    while (remainingGates.isNotEmpty()) {
        var progress = false
        val newRemainingGates = mutableListOf<Gate>()
        for (gate in remainingGates) {
            val val1 = wires[gate.input1]
            val val2 = wires[gate.input2]
            if (val1 != null && val2 != null) {
                val outputVal = when (gate.operation) {
                    "AND" -> if (val1 == 1 && val2 == 1) 1 else 0
                    "OR" -> if (val1 == 1 || val2 == 1) 1 else 0
                    "XOR" -> if (val1 != val2) 1 else 0
                    else -> throw RuntimeException("Unknown operation: ${gate.operation}")
                }
                wires[gate.output] = outputVal
                progress = true
            } else {
                newRemainingGates.add(gate)
            }
        }
        if (!progress) throw RuntimeException("Cannot evaluate remaining gates")
        remainingGates.clear()
        remainingGates.addAll(newRemainingGates)
    }

    val zWires = wires.filterKeys { it.startsWith("z") }.mapKeys { it.key.substring(1).toInt() }
    val indices = zWires.keys.sortedDescending()
    val binaryString = indices.joinToString("") { zWires[it].toString() }
    println(binaryString.toLong(2))
}
