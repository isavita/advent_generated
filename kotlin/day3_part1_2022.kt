import java.io.File

fun itemPriority(item: Char): Int {
    return if (item in 'a'..'z') {
        item - 'a' + 1
    } else {
        item - 'A' + 27
    }
}

fun main(args: Array<String>) {
    var sum = 0
    val file = File("input.txt")
    file.forEachLine {
        val half = it.length / 2
        val firstCompartment = it.substring(0, half)
        val secondCompartment = it.substring(half)

        val compartmentMap = mutableMapOf<Char, Int>()
        firstCompartment.forEach { item ->
            compartmentMap[item] = compartmentMap.getOrDefault(item, 0) + 1
        }
        secondCompartment.forEach { item ->
            if (compartmentMap.containsKey(item)) {
                sum += itemPriority(item)
                return@forEachLine
            }
        }
    }
    println(sum)
}