
import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val (order, time) = solve(input)
    println("Part 1: $order")
    println("Part 2: $time")
}

fun solve(input: List<String>): Pair<String, Int> {
    val (pre, suc) = parseInput(input)
    val allSteps = pre.keys + suc.keys
    val order = part1(allSteps, pre, suc)
    val time = part2(allSteps, pre, suc, 5, 60)
    return Pair(order, time)
}

fun parseInput(input: List<String>): Pair<Map<Char, Set<Char>>, Map<Char, Set<Char>>> {
    val pre = mutableMapOf<Char, MutableSet<Char>>()
    val suc = mutableMapOf<Char, MutableSet<Char>>()
    for (line in input) {
        val words = line.split(" ")
        val p = words[1][0]
        val s = words[7][0]
        pre.getOrPut(s) { mutableSetOf() }.add(p)
        suc.getOrPut(p) { mutableSetOf() }.add(s)
    }
    return Pair(pre, suc)
}

fun part1(allSteps: Set<Char>, pre: Map<Char, Set<Char>>, suc: Map<Char, Set<Char>>): String {
    val done = mutableSetOf<Char>()
    val order = StringBuilder()
    while (done.size < allSteps.size) {
        val ready = allSteps.filter { it !in done && pre[it]?.all { p -> p in done } ?: true }.sorted()
        val next = ready.first()
        done.add(next)
        order.append(next)
    }
    return order.toString()
}

fun part2(
    allSteps: Set<Char>,
    pre: Map<Char, Set<Char>>,
    suc: Map<Char, Set<Char>>,
    workers: Int,
    baseTime: Int
): Int {
    val done = mutableSetOf<Char>()
    val working = mutableMapOf<Char, Int>()
    var time = 0
    while (done.size < allSteps.size) {
        val ready = allSteps.filter {
            it !in done && it !in working && pre[it]?.all { p -> p in done } ?: true
        }.sorted()
        val canStart = ready.take(workers - working.size)
        for (step in canStart) {
            working[step] = baseTime + (step - 'A' + 1)
        }
        val minTime = working.values.minOrNull() ?: 0
        time += minTime
        val finishedThisRound = working.filter { it.value == minTime }.keys
        for (step in finishedThisRound) {
            done.add(step)
            working.remove(step)
        }
        for ((step, timeLeft) in working) {
            working[step] = timeLeft - minTime
        }
    }
    return time
}
