
import java.io.File
import kotlin.math.abs

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}

fun parseInput(input: List<String>): Network {
    val instructions = input[0]
    val nodes = mutableMapOf<String, Pair<String, String>>()
    input.drop(2).forEach { line ->
        val (head, children) = parseLine(line)
        nodes[head] = children
    }
    return Network(instructions, nodes)
}

fun parseLine(line: String): Pair<String, Pair<String, String>> {
    val parts = line.split(" = ")
    val head = parts[0]
    val childrenTrim = parts[1].removeSurrounding("(", ")")
    val childrenParts = childrenTrim.split(", ")
    return head to (childrenParts[0] to childrenParts[1])
}

fun gcd(a: Long, b: Long): Long {
    var x = a
    var y = b
    while (y != 0L) {
        val temp = y
        y = x % y
        x = temp
    }
    return abs(x)
}

fun lcm(a: Long, b: Long): Long {
    return (a * b) / gcd(a, b)
}

fun lcmSlice(nums: List<Long>): Long {
    if (nums.isEmpty()) return 0
    var res = nums[0]
    for (i in 1 until nums.size) {
        res = lcm(res, nums[i])
    }
    return res
}

fun solve(input: List<String>): Long {
    val network = parseInput(input)
    val starts = network.nodes.keys.filter { it.endsWith('A') }
    val steps = starts.map { start ->
        var element = start
        var step = 0L
        while (!element.endsWith('Z')) {
            val instruction = network.instructions[(step % network.instructions.length).toInt()]
            element = if (instruction == 'L') {
                network.nodes[element]!!.first
            } else {
                network.nodes[element]!!.second
            }
            step++
        }
        step
    }
    return lcmSlice(steps)
}

data class Network(val instructions: String, val nodes: Map<String, Pair<String, String>>)
