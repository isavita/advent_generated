
import java.io.File
import java.util.*

data class Gate(val a: String, val op: String, val b: String)

fun parse(input: String): List<Pair<Gate, String>>? {
    val parts = input.split("\n\n")
    if (parts.size != 2) return null

    val gates = mutableListOf<Pair<Gate, String>>()
    for (line in parts[1].lines()) {
        if (line.isEmpty()) continue
        val parts2 = line.split(" -> ")
        if (parts2.size != 2) continue
        val gateParts = parts2[0].split(" ")
        if (gateParts.size != 3) continue
        gates.add(Pair(Gate(gateParts[0], gateParts[1], gateParts[2]), parts2[1]))
    }
    return gates
}

fun createLookups(gates: List<Pair<Gate, String>>): Pair<Map<String, Gate>, Map<String, String>> {
    val lookup = mutableMapOf<String, Gate>()
    val reverseLookup = mutableMapOf<String, String>()

    for (g in gates) {
        lookup[g.second] = g.first
        val inputs = listOf(g.first.a, g.first.b).sorted()
        val key = "${inputs[0]}_${g.first.op}_${inputs[1]}"
        reverseLookup[key] = g.second
    }
    return Pair(lookup, reverseLookup)
}

fun swap(pairs: MutableList<Pair<String, String>>, gates: MutableList<Pair<Gate, String>>, a: String, b: String) {
    pairs.add(Pair(a, b))
    for (i in gates.indices) {
        if (gates[i].second == a) {
            gates[i] = gates[i].copy(second = b)
        } else if (gates[i].second == b) {
            gates[i] = gates[i].copy(second = a)
        }
    }
}

fun getReverseLookupKey(a: String, op: String, b: String): String {
    val inputs = listOf(a, b).sorted()
    return "${inputs[0]}_${op}_${inputs[1]}"
}

fun solution(gates: MutableList<Pair<Gate, String>>): String {
    val pairs = mutableListOf<Pair<String, String>>()
    val numZ = gates.count { it.second.startsWith("z") }

    while (pairs.size < 4) {
        var adder = ""
        var carry = ""
        val (lookup, reverseLookup) = createLookups(gates)

        for (i in 0 until numZ) {
            val xi = "x%02d".format(i)
            val yi = "y%02d".format(i)
            val zi = "z%02d".format(i)

            if (i == 0) {
                adder = reverseLookup[getReverseLookupKey(xi, "XOR", yi)] ?: ""
                carry = reverseLookup[getReverseLookupKey(xi, "AND", yi)] ?: ""
            } else {
                val bit = reverseLookup[getReverseLookupKey(xi, "XOR", yi)] ?: ""
                if (bit.isNotEmpty()) {
                    adder = reverseLookup[getReverseLookupKey(bit, "XOR", carry)] ?: ""
                    if (adder.isNotEmpty()) {
                        val c1 = reverseLookup[getReverseLookupKey(xi, "AND", yi)] ?: ""
                        val c2 = reverseLookup[getReverseLookupKey(bit, "AND", carry)] ?: ""
                        carry = reverseLookup[getReverseLookupKey(c1, "OR", c2)] ?: ""
                    }
                }
            }

            if (adder.isEmpty()) {
                val gate = lookup[zi]!!
                val bitKey = getReverseLookupKey(xi, "XOR", yi)
                val bit = reverseLookup[bitKey] ?: ""
                if (reverseLookup[getReverseLookupKey(gate.a, "XOR", carry)] != null) {
                    swap(pairs, gates, bit, gate.a)
                    break
                } else if (reverseLookup[getReverseLookupKey(gate.b, "XOR", carry)] != null) {
                    swap(pairs, gates, bit, gate.b)
                    break
                }
            } else if (adder != zi) {
                swap(pairs, gates, adder, zi)
                break
            }
        }
    }

    val result = pairs.flatMap { listOf(it.first, it.second) }.sorted()
    return result.joinToString(",")
}

fun main() {
    val input = File("input.txt").readText()
    val gates = parse(input)?.toMutableList()
    if (gates == null) {
        println("Error parsing input")
        return
    }
    println(solution(gates))
}
