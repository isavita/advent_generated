import java.io.File

fun main() {
    val deckSize = 10007
    var position = 2019

    File("input.txt").readLines().forEach { line ->
        when {
            line == "deal into new stack" -> position = deckSize - 1 - position
            line.startsWith("cut") -> {
                val n = line.split(" ").last().toInt()
                position = (position - n + deckSize) % deckSize
            }
            line.startsWith("deal with increment") -> {
                val n = line.split(" ").last().toInt()
                position = (position * n) % deckSize
            }
        }
    }

    println(position)
}