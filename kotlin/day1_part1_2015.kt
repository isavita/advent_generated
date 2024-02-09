import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()
    var floor = 0
    input.forEach {
        if (it == '(') {
            floor++
        } else if (it == ')') {
            floor--
        }
    }
    println(floor)
}