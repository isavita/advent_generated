import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    var nice = 0
    val passesRule1 = fun(line: String): Boolean {
        for (i in 0 until line.length - 2) {
            val toMatch = line.substring(i, i + 2)
            for (j in i + 2 until line.length - 1) {
                if (line.substring(j, j + 2) == toMatch) {
                    return true
                }
            }
        }
        return false
    }

    input.split("\n").forEach { line ->
        var rule1 = passesRule1(line)

        var rule2 = false
        for (i in 0 until line.length - 2) {
            if (line[i] == line[i + 2]) {
                rule2 = true
                break
            }
        }
        if (rule1 && rule2) {
            nice++
        }
    }

    println(nice)
}