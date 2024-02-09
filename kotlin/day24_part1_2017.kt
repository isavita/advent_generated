import java.io.File

data class Component(val a: Int, val b: Int)

var maxStrength = 0

fun findStrongestBridge(components: List<Component>, used: BooleanArray, port: Int, strength: Int) {
    if (strength > maxStrength) {
        maxStrength = strength
    }

    for (i in components.indices) {
        if (used[i]) {
            continue
        }

        val c = components[i]
        if (c.a == port || c.b == port) {
            used[i] = true
            val nextPort = if (c.a == port) c.b else c.a
            findStrongestBridge(components, used, nextPort, strength + c.a + c.b)
            used[i] = false
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
    findStrongestBridge(components, used, 0, 0)

    println(maxStrength)
}