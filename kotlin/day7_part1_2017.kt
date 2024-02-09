import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()

    val holderMap = mutableMapOf<String, Boolean>()
    val heldMap = mutableMapOf<String, Boolean>()

    val regex = Regex("[a-z]+")

    lines.forEach { line ->
        val names = regex.findAll(line).map { it.value }.toList()
        val holder = names[0]
        holderMap[holder] = true

        if (names.size > 1) {
            names.subList(1, names.size).forEach { name ->
                heldMap[name] = true
            }
        }
    }

    holderMap.keys.forEach { holder ->
        if (!heldMap.containsKey(holder)) {
            println(holder)
            return
        }
    }
}