import java.io.File
import java.util.*

fun main() {
    val root = listOf("")
    val dirs = mutableMapOf<String, Int>()
    val files = mutableMapOf<String, Int>()
    var curr = mutableListOf<String>()

    File("input.txt").forEachLine { line ->
        val txt = line.split(" ")
        if (txt[0] == "$") {
            if (txt[1] == "cd") {
                if (txt[2] == "/") {
                    curr = root.toMutableList()
                } else if (txt[2] == "..") {
                    curr = curr.dropLast(1).toMutableList()
                } else {
                    curr.add(txt[2])
                }
                dirs[curr.joinToString("/")] = 0
            }
        } else {
            if (txt[0] != "dir") {
                files[curr.plus(txt[1]).joinToString("/")] = txt[0].toInt()
            }
        }
    }

    for ((f, s) in files) {
        val path = f.split("/")
        for (i in 1 until path.size) {
            dirs[path.take(i).joinToString("/")] = dirs[path.take(i).joinToString("/")]!! + s
        }
    }

    val sortedSizes = dirs.values.sorted()
    val total = 70000000
    val want = 30000000
    val available = total - dirs[""]!!
    println(sortedSizes.first { it >= want - available })
}