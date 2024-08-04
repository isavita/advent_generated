import java.io.File

fun main() {
    val input = File("input.txt").readLines().map { it.toInt() }
    val mixedList = mix(input)
    val groveCoordinates = findGroveCoordinates(mixedList)
    println(groveCoordinates.sum())
}

fun mix(input: List<Int>): List<Int> {
    val list = input.toMutableList()
    val size = list.size
    val indices = list.indices.toMutableList()

    for (originalIndex in input.indices) {
        val currentIndex = indices.indexOf(originalIndex)
        val value = list.removeAt(currentIndex)
        indices.removeAt(currentIndex)

        var newIndex = (currentIndex + value).mod(size - 1)
        if (newIndex <= 0) newIndex += size - 1

        list.add(newIndex, value)
        indices.add(newIndex, originalIndex)
    }

    return list
}

fun findGroveCoordinates(list: List<Int>): List<Int> {
    val zeroIndex = list.indexOf(0)
    val size = list.size
    return listOf(
        list[(zeroIndex + 1000).mod(size)],
        list[(zeroIndex + 2000).mod(size)],
        list[(zeroIndex + 3000).mod(size)]
    )
}

fun Int.mod(n: Int): Int = ((this % n) + n) % n