fun main() {
    val claims = mutableListOf<String>()
    val fabric = Array(1000) { IntArray(1000) }

    val lines = mutableListOf<String>()
    java.io.File("input.txt").forEachLine { lines.add(it) }

    for (line in lines) {
        val parts = line.split(" ")
        val start = parts[2].split(",")
        val size = parts[3].split("x")

        val x = start[0].toInt()
        val y = start[1].substring(0, start[1].length - 1).toInt()
        val width = size[0].toInt()
        val height = size[1].toInt()

        for (i in x until x + width) {
            for (j in y until y + height) {
                fabric[i][j]++
            }
        }
    }

    var count = 0
    for (i in fabric.indices) {
        for (j in fabric[i].indices) {
            if (fabric[i][j] >= 2) {
                count++
            }
        }
    }

    println(count)
}