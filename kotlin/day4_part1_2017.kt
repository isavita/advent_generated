import java.io.File

fun main(args: Array<String>) {
    val passphrases = File("input.txt").readLines()
    var validCount = 0

    passphrases.forEach { passphrase ->
        val words = passphrase.split(" ")
        val wordSet = mutableSetOf<String>()

        var valid = true
        words.forEach { word ->
            if (wordSet.contains(word)) {
                valid = false
                return@forEach
            }
            wordSet.add(word)
        }

        if (valid) {
            validCount++
        }
    }

    println(validCount)
}