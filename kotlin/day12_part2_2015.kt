
import java.io.File

fun main() {
    val input = File("input.txt").readText()

    println(sumNumbers(input))
    println(sumNumbersWithoutRed(input))
}

fun sumNumbers(json: String): Int {
    val regex = Regex("-?\\d+")
    return regex.findAll(json).sumOf { it.value.toInt() }
}

fun sumNumbersWithoutRed(json: String): Int {
    fun process(obj: Any?): Int {
        return when (obj) {
            is Map<*, *> -> {
                if (obj.values.any { it == "red" }) 0
                else obj.values.sumOf { process(it) }
            }
            is List<*> -> obj.sumOf { process(it) }
            is Number -> obj.toInt()
            else -> 0
        }
    }

    val parsed = parseJson(json)
    return process(parsed)
}

// Simple JSON parser (handles only basic cases for this problem)
fun parseJson(json: String): Any? {
    val trimmed = json.trim()
    return when {
        trimmed.startsWith("{") -> {
            val map = mutableMapOf<String, Any?>()
            val content = trimmed.substring(1, trimmed.length - 1)
            if (content.isNotEmpty()) {
                parsePairs(content).forEach { (key, value) ->
                    map[key] = parseJson(value)
                }
            }
            map
        }
        trimmed.startsWith("[") -> {
            val list = mutableListOf<Any?>()
            val content = trimmed.substring(1, trimmed.length - 1)
            if (content.isNotEmpty()) {
                parseElements(content).forEach {
                    list.add(parseJson(it))
                }
            }
            list
        }
        trimmed.matches(Regex("-?\\d+")) -> trimmed.toInt()
        else -> trimmed.removeSurrounding("\"")
    }
}

fun parsePairs(content: String): List<Pair<String, String>> {
    val pairs = mutableListOf<Pair<String, String>>()
    var start = 0
    var depth = 0
    var inString = false
    var i = 0
    while (i < content.length) {
        when (content[i]) {
            '"' -> if (i == 0 || content[i - 1] != '\\') inString = !inString
            '{', '[' -> if (!inString) depth++
            '}', ']' -> if (!inString) depth--
            ',' -> if (depth == 0 && !inString) {
                val pair = content.substring(start, i).split(":", limit = 2)
                pairs.add(pair[0].removeSurrounding("\"").trim() to pair[1].trim())
                start = i + 1
            }
        }
        i++
    }
    val pair = content.substring(start).split(":", limit = 2)
    pairs.add(pair[0].removeSurrounding("\"").trim() to pair[1].trim())
    return pairs
}

fun parseElements(content: String): List<String> {
    val elements = mutableListOf<String>()
    var start = 0
    var depth = 0
    var inString = false
    var i = 0
    while (i < content.length) {
        when (content[i]) {
            '"' -> if (i == 0 || content[i - 1] != '\\') inString = !inString
            '{', '[' -> if (!inString) depth++
            '}', ']' -> if (!inString) depth--
            ',' -> if (depth == 0 && !inString) {
                elements.add(content.substring(start, i).trim())
                start = i + 1
            }
        }
        i++
    }
    elements.add(content.substring(start).trim())
    return elements
}
