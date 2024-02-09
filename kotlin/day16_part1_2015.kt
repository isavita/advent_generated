import java.io.File

fun main(args: Array<String>) {
    val mfcsam = mapOf(
        "children" to 3, "cats" to 7, "samoyeds" to 2, "pomeranians" to 3,
        "akitas" to 0, "vizslas" to 0, "goldfish" to 5, "trees" to 3,
        "cars" to 2, "perfumes" to 1
    )

    val file = File("input.txt")
    val lines = file.readLines()

    for (line in lines) {
        val parts = line.split(" ")
        val sueNumber = parts[1].substringBeforeLast(':')

        var matches = true
        var i = 2
        while (i < parts.size) {
            val item = parts[i].substringBeforeLast(':')
            val count = parts[i + 1].substringBeforeLast(',')
            if (mfcsam[item] != count.toInt()) {
                matches = false
                break
            }
            i += 2
        }

        if (matches) {
            println(sueNumber)
            break
        }
    }
}