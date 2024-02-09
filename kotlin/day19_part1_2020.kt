import java.io.File
import java.util.regex.Pattern

fun main(args: Array<String>) {
    val rules = readRules(File("input.txt").bufferedReader())
    val pattern = constructPattern(rules, 0)
    val count = countMatches(File("input.txt").bufferedReader(), pattern)
    println("The number of messages that completely match rule 0 is: $count")
}

fun readRules(reader: java.io.BufferedReader): Map<Int, String> {
    val rules = mutableMapOf<Int, String>()
    while (true) {
        val line = reader.readLine() ?: break
        if (line.isEmpty()) break
        val parts = line.split(": ")
        rules[parts[0].toInt()] = parts[1].replace("\"", "")
    }
    return rules
}

fun constructPattern(rules: Map<Int, String>, index: Int): String {
    if (rules[index]!!.contains("|")) {
        val subrules = rules[index]!!.split(" | ")
        val parts = subrules.map { constructSubPattern(rules, it) }
        return "(${parts.joinToString("|")})"
    }
    return constructSubPattern(rules, rules[index]!!)
}

fun constructSubPattern(rules: Map<Int, String>, subrule: String): String {
    if (subrule == "a" || subrule == "b") {
        return subrule
    }
    val subIdxs = subrule.split(" ")
    var pattern = ""
    for (idx in subIdxs) {
        pattern += constructPattern(rules, idx.toInt())
    }
    return pattern
}

fun countMatches(reader: java.io.BufferedReader, pattern: String): Int {
    val re = Pattern.compile("^$pattern$")
    var count = 0
    while (true) {
        val message = reader.readLine() ?: break
        if (re.matcher(message).matches()) {
            count++
        }
    }
    return count
}