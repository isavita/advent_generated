import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readLines().first()

    val repeatedInput = repeatInput(input, 10000)

    val offset = input.substring(0, 7).toInt()

    repeat(100) {
        var sum = 0
        for (i in repeatedInput.size - 1 downTo offset) {
            sum += repeatedInput[i]
            repeatedInput[i] = sum % 10
        }
    }

    repeat(8) {
        print(repeatedInput[offset + it])
    }
    println()
}

fun repeatInput(input: String, times: Int): MutableList<Int> {
    val digits = mutableListOf<Int>()
    repeat(times) {
        input.forEach { char ->
            digits.add(Character.getNumericValue(char))
        }
    }
    return digits
}