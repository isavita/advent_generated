
import java.io.File

data class Valve(val id: String, val flow: Int, val tunnels: MutableMap<String, Int>)

fun maxPressure(
    valves: Map<String, Valve>,
    curr: String,
    minute: Int,
    pressure: Int,
    open: List<String>
): Int {
    var maxPressureValue = pressure
    for (nextValve in open) {
        val newOpen = open.filter { it != nextValve }
        val timeLeft = minute - valves[curr]!!.tunnels[nextValve]!! - 1
        if (timeLeft > 0) {
            maxPressureValue = maxOf(
                maxPressureValue,
                maxPressure(
                    valves,
                    nextValve,
                    timeLeft,
                    timeLeft * valves[nextValve]!!.flow + pressure,
                    newOpen
                )
            )
        }
    }
    return maxPressureValue
}

fun divide(l: Int): List<Pair<List<Int>, List<Int>>> {
    if (l == 1) {
        return listOf(Pair(emptyList(), listOf(0)), Pair(listOf(0), emptyList()))
    }
    val d = divide(l - 1)
    val r = mutableListOf<Pair<List<Int>, List<Int>>>()
    for (i in d.indices) {
        r.add(Pair(d[i].first + listOf(l - 1), d[i].second))
        r.add(Pair(d[i].first, d[i].second + listOf(l - 1)))
    }
    return r
}

fun main() {
    val inputData = File("input.txt").readText().trim()
    val valves = mutableMapOf<String, Valve>()
    for (line in inputData.split("\n")) {
        val sp = line.split("; ")
        val id = sp[0].split(" ")[1]
        val flow = sp[0].split("=")[1].toInt()
        val tunnels = sp[1].replace("tunnel leads to valve", "").replace("tunnels lead to valves", "").trim()
            .split(", ").associateWith { 1 }.toMutableMap()
        tunnels[id] = 0
        valves[id] = Valve(id, flow, tunnels)
    }

    for (k in valves.keys) {
        for (i in valves.keys) {
            for (j in valves.keys) {
                if (valves[i]!!.tunnels.containsKey(k) && valves[k]!!.tunnels.containsKey(j)) {
                    if (!valves[i]!!.tunnels.containsKey(j) || valves[i]!!.tunnels[j]!! > valves[i]!!.tunnels[k]!! + valves[k]!!.tunnels[j]!!) {
                        valves[i]!!.tunnels[j] = valves[i]!!.tunnels[k]!! + valves[k]!!.tunnels[j]!!
                    }
                }
            }
        }
    }

    val openValves = valves.values.filter { it.flow > 0 }.map { it.id }
    var maxTotalPressureValue = 0
    for (d in divide(openValves.size)) {
        if (d.first.isEmpty() || d.second.isEmpty()) continue
        val mine = d.first.map { openValves[it] }
        val elephant = d.second.map { openValves[it] }
        val totalPressure =
            maxPressure(valves, "AA", 26, 0, mine) + maxPressure(valves, "AA", 26, 0, elephant)
        maxTotalPressureValue = maxOf(maxTotalPressureValue, totalPressure)
    }
    println(maxTotalPressureValue)
}
