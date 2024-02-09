import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    var twoCount = 0
    var threeCount = 0

    for (line in lines) {
        val (twos, threes) = countTwosAndThrees(line)
        if (twos) {
            twoCount++
        }
        if (threes) {
            threeCount++
        }
    }

    val checksum = twoCount * threeCount
    println(checksum)
}

fun countTwosAndThrees(id: String): Pair<Boolean, Boolean> {
    val charCount = mutableMapOf<Char, Int>()
    for (char in id) {
        charCount[char] = charCount.getOrDefault(char, 0) + 1
    }

    var hasTwos = false
    var hasThrees = false
    for (count in charCount.values) {
        if (count == 2) {
            hasTwos = true
        } else if (count == 3) {
            hasThrees = true
        }
    }
    return Pair(hasTwos, hasThrees)
}