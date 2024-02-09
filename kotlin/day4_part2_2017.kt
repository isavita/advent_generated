import java.io.File

fun main() {
    val passphrases = File("input.txt").readLines()

    val validPassphrasesPart1 = passphrases.filter { isValidPassphrasePart1(it) }.size
    val validPassphrasesPart2 = passphrases.filter { isValidPassphrasePart2(it) }.size

    println("Part 1: $validPassphrasesPart1")
    println("Part 2: $validPassphrasesPart2")
}

fun isValidPassphrasePart1(passphrase: String): Boolean {
    val words = passphrase.split(" ")
    return words.toSet().size == words.size
}

fun isValidPassphrasePart2(passphrase: String): Boolean {
    val words = passphrase.split(" ")
    val sortedWords = words.map { it.toCharArray().sorted().joinToString("") }
    return sortedWords.toSet().size == sortedWords.size
}