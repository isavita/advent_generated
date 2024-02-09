import java.io.File

fun main(args: Array<String>) {
    val contains = mutableMapOf<String, MutableList<String>>()
    File("input.txt").forEachLine {
        val parts = it.split(" bags contain ")
        val container = parts[0]
        if (parts[1] == "no other bags.") {
            return@forEachLine
        }
        val containedBags = parts[1].split(", ")
        for (bag in containedBags) {
            val bagName = bag.split(" ")[1] + " " + bag.split(" ")[2]
            if (contains[bagName] == null) {
                contains[bagName] = mutableListOf()
            }
            contains[bagName]!!.add(container)
        }
    }

    val count = countCanContain("shiny gold", contains)
    println(count)
}

fun countCanContain(target: String, contains: Map<String, List<String>>): Int {
    val seen = mutableMapOf<String, Boolean>()
    fun dfs(bag: String) {
        for (outer in contains[bag] ?: emptyList()) {
            if (seen[outer] != true) {
                seen[outer] = true
                dfs(outer)
            }
        }
    }
    dfs(target)
    return seen.size
}