import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().split("\n\n")
    val inputLines = input[0].split("\n")
    val stacks = Array((inputLines[0].length + 1) / 4) { mutableListOf<Char>() }

    inputLines.forEach { line ->
        line.forEachIndexed { index, char ->
            if (char in 'A'..'Z') {
                stacks[(index - 1) / 4].add(char)
            }
        }
    }

    val steps = input[1].split("\n")
    println(move(stacks, steps))
}

fun move(stacks: Array<MutableList<Char>>, steps: List<String>): String {
    val reversedStacks = Array(stacks.size) { mutableListOf<Char>() }
    stacks.forEachIndexed { index, stack ->
        stack.reversed().forEach { char ->
            reversedStacks[index].add(char)
        }
    }

    steps.forEach { step ->
        val (n, from, to) = Regex("""move (\d+) from (\d+) to (\d+)""").find(step)!!.destructured
        val nInt = n.toInt()
        val fromIndex = from.toInt() - 1
        val toIndex = to.toInt() - 1
        reversedStacks[toIndex].addAll(reversedStacks[fromIndex].takeLast(nInt))
        reversedStacks[fromIndex] = reversedStacks[fromIndex].dropLast(nInt).toMutableList()
    }

    return reversedStacks.map { it.last() }.joinToString("")
}