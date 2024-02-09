import java.io.File

fun main(args: Array<String>) {
    val (deps, allSteps) = parseInput("input.txt")
    val order = topologicalSort(deps, allSteps)
    println(order)
}

fun parseInput(filename: String): Pair<MutableMap<Char, MutableList<Char>>, MutableMap<Char, Boolean>> {
    val deps = mutableMapOf<Char, MutableList<Char>>()
    val allSteps = mutableMapOf<Char, Boolean>()

    File(filename).forEachLine {
        val parts = it.split(" ")
        val a = parts[1].single()
        val b = parts[7].single()
        deps.computeIfAbsent(b) { mutableListOf() }.add(a)
        allSteps[a] = true
        allSteps[b] = true
    }

    return Pair(deps, allSteps)
}

fun topologicalSort(deps: MutableMap<Char, MutableList<Char>>, allSteps: MutableMap<Char, Boolean>): String {
    val order = mutableListOf<Char>()
    val available = mutableListOf<Char>()

    for (step in allSteps.keys) {
        if (deps[step].isNullOrEmpty()) {
            available.add(step)
        }
    }
    available.sort()

    while (available.isNotEmpty()) {
        val next = available.removeAt(0)
        order.add(next)

        for (step in allSteps.keys) {
            if (deps[step]?.contains(next) == true) {
                deps[step]?.remove(next)
                if (deps[step].isNullOrEmpty()) {
                    available.add(step)
                }
            }
        }
        available.sort()
    }
    return order.joinToString("")
}