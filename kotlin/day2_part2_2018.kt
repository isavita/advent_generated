import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    for (i in 0 until lines.size - 1) {
        for (j in i + 1 until lines.size) {
            var diff = 0
            for (k in lines[i].indices) {
                if (lines[i][k] != lines[j][k]) {
                    diff++
                    if (diff > 1) {
                        break
                    }
                }
            }
            if (diff == 1) {
                var common = ""
                for (k in lines[i].indices) {
                    if (lines[i][k] == lines[j][k]) {
                        common += lines[i][k]
                    }
                }
                println(common)
                return
            }
        }
    }
}