import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    var totalCount = 0
    val groupAnswers = mutableSetOf<Char>()

    for (line in lines) {
        if (line.isEmpty()) {
            totalCount += groupAnswers.size
            groupAnswers.clear()
        } else {
            line.forEach { groupAnswers.add(it) }
        }
    }

    totalCount += groupAnswers.size
    println(totalCount)
}