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
    var groupLineCounter = 0
    val groupItems = Array(3) { mutableMapOf<Char, Int>() }

    File("input.txt").forEachLine { line ->
        val itemsMap = mutableMapOf<Char, Int>()
        line.forEach { item ->
            itemsMap[item] = itemsMap.getOrDefault(item, 0) + 1
        }
        groupItems[groupLineCounter] = itemsMap
        groupLineCounter++

        if (groupLineCounter == 3) {
            val commonItems = mutableMapOf<Char, Int>()
            for (item in groupItems[0].keys) {
                if (groupItems[1].getOrDefault(item, 0) > 0 && groupItems[2].getOrDefault(item, 0) > 0) {
                    commonItems[item] = 1
                }
            }
            for (item in commonItems.keys) {
                sum += itemPriority(item)
                break
            }
            groupLineCounter = 0
        }
    }

    println(sum)
}