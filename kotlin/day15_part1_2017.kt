import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    val genAStart = input[0].toLong()
    val genBStart = input[1].toLong()

    val genAFactor = 16807L
    val genBFactor = 48271L
    val modulus = 2147483647L

    var genA = genAStart
    var genB = genBStart
    var matches = 0

    repeat(40000000) {
        genA = (genA * genAFactor) % modulus
        genB = (genB * genBFactor) % modulus

        if (genA and 0xFFFF == genB and 0xFFFF) {
            matches++
        }
    }

    println(matches)
}