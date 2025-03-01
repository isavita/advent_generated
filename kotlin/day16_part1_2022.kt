
import java.io.File

fun main() {
    val valves = mutableMapOf<String, Valve>()
    File("input.txt").readLines().forEach { line ->
        val parts = line.split("; ")
        val id = parts[0].split(" ")[1]
        val flow = parts[0].split("=")[1].toInt()
        val tunnels = mutableMapOf(id to 0)
        if (parts[1].startsWith("tunnels lead to valves ")) {
            parts[1].substring(23).split(", ").forEach { tunnels[it] = 1 }
        } else {
            parts[1].substring(22).split(", ").forEach { tunnels[it] = 1 }
        }
        valves[id] = Valve(id, flow, tunnels)
    }

    for (k in valves.keys) {
        for (i in valves.keys) {
            for (j in valves.keys) {
                val dik = valves[i]!!.tunnels[k] ?: continue
                val dkj = valves[k]!!.tunnels[j] ?: continue
                val dij = valves[i]!!.tunnels.getOrDefault(j, Int.MAX_VALUE)
                if (dij > dik + dkj) {
                    valves[i]!!.tunnels[j] = dik + dkj
                }
            }
        }
    }

    val openValves = valves.filter { it.value.flow > 0 }.keys.toList()
    println(maxPressure(valves, "AA", 30, 0, openValves))
}

fun maxPressure(valves: Map<String, Valve>, curr: String, minute: Int, pressure: Int, open: List<String>): Int {
    var maxVal = pressure
    for (nextValve in open) {
        val newOpen = open.filter { it != nextValve }
        val timeLeft = minute - (valves[curr]!!.tunnels[nextValve] ?: continue) - 1
        if (timeLeft > 0) {
            maxVal = maxOf(maxVal, maxPressure(valves, nextValve, timeLeft, timeLeft * valves[nextValve]!!.flow + pressure, newOpen))
        }
    }
    return maxVal
}

data class Valve(val id: String, val flow: Int, val tunnels: MutableMap<String, Int>)
