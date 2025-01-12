
import java.io.File

fun main() {
    val input = File("input.txt").readLines().map { it.toLong() }

    fun mix(numbers: List<Long>, mixCount: Int = 1, decryptionKey: Long = 1): List<Long> {
        val indexedNumbers = numbers.mapIndexed { index, value -> IndexedValue(index, value * decryptionKey) }
        var currentList = indexedNumbers.toMutableList()

        repeat(mixCount) {
            for (originalIndex in indexedNumbers.indices) {
                val currentIndex = currentList.indexOfFirst { it.index == originalIndex }
                val currentValue = currentList[currentIndex]
                val move = currentValue.value
                currentList.removeAt(currentIndex)

                var newIndex = (currentIndex + move) % currentList.size
                if (newIndex < 0) newIndex += currentList.size
                currentList.add(newIndex.toInt(), currentValue)
            }
        }
        return currentList.map { it.value }
    }

    fun calculateGroveCoordinates(mixedList: List<Long>): Long {
        val zeroIndex = mixedList.indexOf(0)
        val size = mixedList.size
        return listOf(1000, 2000, 3000).sumOf { mixedList[(zeroIndex + it) % size] }
    }

    // Part 1
    val mixedPart1 = mix(input)
    println("Part 1: ${calculateGroveCoordinates(mixedPart1)}")

    // Part 2
    val mixedPart2 = mix(input, 10, 811589153)
    println("Part 2: ${calculateGroveCoordinates(mixedPart2)}")
}

data class IndexedValue(val index: Int, val value: Long)
