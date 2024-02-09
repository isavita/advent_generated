import java.io.File

fun main(args: Array<String>) {
    val s = File("input.txt").readText().split("\n\n")
    val input = s[0].split("\n")
    val stacks = Array((input[0].length + 1) / 4) { ArrayList<Char>() }
    
    input.forEach { line ->
        line.forEachIndexed { i, b ->
            if (b in 'A'..'Z') {
                stacks[(i - 1) / 4].add(b)
            }
        }
    }
    
    val steps = s[1].split("\n")
    println(move(stacks, steps))
}

fun move(st: Array<ArrayList<Char>>, steps: List<String>): String {
    val stacks = Array(st.size) { ArrayList<Char>() }
    
    st.forEachIndexed { i, stack ->
        stack.reversed().forEach { stacks[i].add(it) }
    }
    
    steps.forEach { step ->
        val (n, from, to) = Regex("""move (\d+) from (\d+) to (\d+)""").find(step)!!.destructured
        val fromIndex = from.toInt() - 1
        val toIndex = to.toInt() - 1
        
        repeat(n.toInt()) {
            stacks[toIndex].add(stacks[fromIndex].removeAt(stacks[fromIndex].size - 1))
        }
    }
    
    return stacks.map { it.last() }.joinToString("")
}