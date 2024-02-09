import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val genAStart = lines[0].toLong()
    val genBStart = lines[1].toLong()

    val genAFactor = 16807L
    val genBFactor = 48271L
    val modulus = 2147483647L

    var genA = genAStart
    var genB = genBStart
    var matches = 0

    repeat(5000000) {
        // Generate next value for A that is a multiple of 4
        while (true) {
            genA = (genA * genAFactor) % modulus
            if (genA % 4 == 0L) break
        }

        // Generate next value for B that is a multiple of 8
        while (true) {
            genB = (genB * genBFactor) % modulus
            if (genB % 8 == 0L) break
        }

        if (genA and 0xFFFF == genB and 0xFFFF) {
            matches++
        }
    }

    println(matches)
}