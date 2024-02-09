import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    var totalCount = 0
    var groupAnswers = mutableMapOf<Char, Int>()
    var groupSize = 0

    for (line in lines) {
        if (line.isEmpty()) {
            for (count in groupAnswers.values) {
                if (count == groupSize) {
                    totalCount++
                }
            }
            groupAnswers = mutableMapOf()
            groupSize = 0
        } else {
            groupSize++
            for (question in line) {
                groupAnswers[question] = groupAnswers.getOrDefault(question, 0) + 1
            }
        }
    }

    for (count in groupAnswers.values) {
        if (count == groupSize) {
            totalCount++
        }
    }

    println(totalCount)
}