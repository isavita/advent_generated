import java.io.File

data class Component(val a: Int, val b: Int)

var maxStrength = 0
var maxLength = 0

fun findStrongestLongestBridge(components: List<Component>, used: BooleanArray, port: Int, strength: Int, length: Int) {
    if (length > maxLength || (length == maxLength && strength > maxStrength)) {
        maxStrength = strength
        maxLength = length
    }

    components.forEachIndexed { index, c ->
        if (used[index]) {
            return@forEachIndexed
        }

        if (c.a == port || c.b == port) {
            used[index] = true
            val nextPort = if (c.a == port) c.b else c.a
            findStrongestLongestBridge(components, used, nextPort, strength + c.a + c.b, length + 1)
            used[index] = false
        }
    }
}

fun main(args: Array<String>) {
    val components = mutableListOf<Component>()
    File("input.txt").forEachLine {
        val ports = it.split("/")
        val a = ports[0].toInt()
        val b = ports[1].toInt()
        components.add(Component(a, b))
    }

    val used = BooleanArray(components.size)
    findStrongestLongestBridge(components, used, 0, 0, 0)

    println(maxStrength)
}