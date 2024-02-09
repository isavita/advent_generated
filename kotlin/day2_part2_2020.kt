import java.io.File

fun main() {
    val lines = File("input.txt").readLines()

    var count1 = 0
    var count2 = 0

    for (line in lines) {
        val parts = line.split(" ")
        val range = parts[0].split("-")
        val min = range[0].toInt()
        val max = range[1].toInt()
        val char = parts[1][0]
        val password = parts[2]

        val occurrences = password.count { it == char }

        if (occurrences in min..max) {
            count1++
        }

        if ((password[min - 1] == char) xor (password[max - 1] == char)) {
            count2++
        }
    }

    println(count1)
    println(count2)
}