
import java.io.File

class Monkey(val name: String) {
    var value: Long? = null
    var left: Monkey? = null
    var right: Monkey? = null
    var op: String? = null
}

fun main() {
    val index = parse("input.txt")
    index["humn"]!!.value = null
    index["root"]!!.op = "=="
    println(index["root"]!!.expect(0))
}

fun Monkey.solve(): Long? {
    if (value != null) return value
    val l = left?.solve()
    val r = right?.solve()
    if (l != null && r != null) {
        value = when (op) {
            "+" -> l + r
            "-" -> l - r
            "*" -> l * r
            "/" -> l / r
            "==" -> if (l == r) 0 else 1
            else -> null
        }
        return value
    }
    return null
}

fun Monkey.expect(x: Long): Long {
    if (name == "humn") return x
    val l = left?.solve()
    val r = right?.solve()
    if (l == null) {
        return left!!.expect(
            when (op) {
                "+" -> x - r!!
                "-" -> x + r!!
                "*" -> x / r!!
                "/" -> x * r!!
                "==" -> r!!
                else -> error("Invalid operator")
            }
        )
    }
    if (r == null) {
        return right!!.expect(
            when (op) {
                "+" -> x - l
                "-" -> l - x
                "*" -> x / l
                "/" -> l / x
                "==" -> l
                else -> error("Invalid operator")
            }
        )
    }
    error("Both sides solved")
}

fun parse(filename: String): MutableMap<String, Monkey> {
    val index = mutableMapOf<String, Monkey>()
    File(filename).readLines().forEach { line ->
        val parts = line.split(": ")
        val name = parts[0]
        val monkey = index.getOrPut(name) { Monkey(name) }
        val value = parts[1].toLongOrNull()
        if (value != null) {
            monkey.value = value
        } else {
            val opParts = parts[1].split(" ")
            monkey.left = index.getOrPut(opParts[0]) { Monkey(opParts[0]) }
            monkey.op = opParts[1]
            monkey.right = index.getOrPut(opParts[2]) { Monkey(opParts[2]) }
        }
    }
    return index
}
