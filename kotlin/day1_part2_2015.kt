import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()
    var floor = 0
    var position = 0
    for ((index, char) in input.withIndex()) {
        if (char == '(') {
            floor++
        } else if (char == ')') {
            floor--
        }
        if (floor == -1) {
            position = index + 1
            break
        }
    }
    println(position)
}