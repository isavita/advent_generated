import java.io.File

fun main() {
    val input = File("input.txt").readLines().map { it.toLong() }

    val preambleSize = 25
    val invalidNumber = findInvalidNumber(input, preambleSize)
    println("Invalid number: $invalidNumber")

    val encryptionWeakness = findEncryptionWeakness(input, invalidNumber)
    println("Encryption weakness: $encryptionWeakness")
}

fun findInvalidNumber(input: List<Long>, preambleSize: Int): Long {
    for (i in preambleSize until input.size) {
        val previousNumbers = input.subList(i - preambleSize, i)
        if (!isValidNumber(input[i], previousNumbers)) {
            return input[i]
        }
    }
    error("No invalid number found")
}

fun isValidNumber(number: Long, previousNumbers: List<Long>): Boolean {
    for (i in previousNumbers.indices) {
        for (j in i + 1 until previousNumbers.size) {
            if (previousNumbers[i] + previousNumbers[j] == number) {
                return true
            }
        }
    }
    return false
}

fun findEncryptionWeakness(input: List<Long>, invalidNumber: Long): Long {
    for (i in input.indices) {
        var sum = 0L
        for (j in i until input.size) {
            sum += input[j]
            if (sum == invalidNumber) {
                val range = input.subList(i, j + 1)
                return range.minOrNull()!! + range.maxOrNull()!!
            } else if (sum > invalidNumber) {
                break
            }
        }
    }
    error("No encryption weakness found")
}