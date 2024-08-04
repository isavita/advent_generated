import java.io.File

fun hashAlgorithm(input: String): Int {
    var currentValue = 0
    for (char in input) {
        currentValue += char.code
        currentValue *= 17
        currentValue %= 256
    }
    return currentValue
}

fun main() {
    val inputFile = File("input.txt")
    val initializationSequence = inputFile.readText().trim()
    val steps = initializationSequence.split(",")

    val results = steps.map { hashAlgorithm(it) }
    val sumOfResults = results.sum()

    println("The sum of the results is: $sumOfResults")
}