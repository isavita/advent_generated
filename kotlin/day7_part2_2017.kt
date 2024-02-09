
import java.io.File

data class Program(val weight: Int, val holds: List<String>)

fun dfs(name: String, programs: Map<String, Program>): Pair<Int, Boolean> {
    val program = programs[name] ?: return Pair(0, false)
    var totalWeight = program.weight

    val weights = mutableMapOf<Int, Int>()
    for (child in program.holds) {
        val (weight, balanced) = dfs(child, programs)
        if (!balanced) {
            return Pair(0, false)
        }
        totalWeight += weight
        weights[weight] = weights.getOrDefault(weight, 0) + 1
    }

    for ((w1, c1) in weights) {
        for ((w2, c2) in weights) {
            if (w1 != w2 && c1 < c2) {
                var unbalancedProgram = ""
                for (child in program.holds) {
                    if (dfs(child, programs).first == w1) {
                        unbalancedProgram = child
                        break
                    }
                }
                println(programs[unbalancedProgram]?.weight?.plus(w2 - w1))
                return Pair(0, false)
            }
        }
    }
    return Pair(totalWeight, true)
}

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()

    val programs = mutableMapOf<String, Program>()

    val regex = Regex("[a-z]+|\\d+")

    for (line in lines) {
        val matches = regex.findAll(line).map { it.value }.toList()
        val name = matches[0]
        val weight = matches[1].toInt()

        val program = Program(weight, if (matches.size > 2) matches.subList(2, matches.size) else emptyList())
        programs[name] = program
    }

    val root = "dtacyn" // Replace this with the root found in Part One

    dfs(root, programs)
}
