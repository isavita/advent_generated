import java.io.File
import java.util.regex.Pattern

fun main() {
    val input = File("input.txt").readText().trim()
    println(someAssemblyRequired(input))
}

fun someAssemblyRequired(input: String): Int {
    val wireToRule = mutableMapOf<String, String>()

    input.split("\n").forEach { inst ->
        val parts = inst.split(" -> ")
        wireToRule[parts[1]] = parts[0]
    }

    val aSignal = memoDFS(wireToRule, "a", mutableMapOf())
    wireToRule["b"] = aSignal.toString()
    return memoDFS(wireToRule, "a", mutableMapOf())
}

fun memoDFS(graph: Map<String, String>, entry: String, memo: MutableMap<String, Int>): Int {
    if (memo.containsKey(entry)) {
        return memo[entry]!!
    }

    if (Pattern.matches("[0-9]+", entry)) {
        return entry.toInt()
    }

    val sourceRule = graph[entry]!!
    val parts = sourceRule.split(" ")

    var result = 0
    when {
        parts.size == 1 -> result = memoDFS(graph, parts[0], memo)
        parts[0] == "NOT" -> {
            val start = memoDFS(graph, parts[1], memo)
            result = (0xFFFF) xor start
        }
        parts[1] == "AND" -> result = memoDFS(graph, parts[0], memo) and memoDFS(graph, parts[2], memo)
        parts[1] == "OR" -> result = memoDFS(graph, parts[0], memo) or memoDFS(graph, parts[2], memo)
        parts[1] == "LSHIFT" -> result = memoDFS(graph, parts[0], memo) shl memoDFS(graph, parts[2], memo)
        parts[1] == "RSHIFT" -> result = memoDFS(graph, parts[0], memo) shr memoDFS(graph, parts[2], memo)
    }

    memo[entry] = result
    return result
}