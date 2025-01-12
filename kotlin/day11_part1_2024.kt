
import java.io.File

fun main() {
    val initialStones = File("input.txt").readText().trim().split(" ").map { it.toLong() }.toMutableList()
    var stones = initialStones

    repeat(25) {
        val nextStones = mutableListOf<Long>()
        for (stone in stones) {
            when {
                stone == 0L -> nextStones.add(1)
                stone.toString().length % 2 == 0 -> {
                    val str = stone.toString()
                    val mid = str.length / 2
                    nextStones.add(str.substring(0, mid).toLong())
                    nextStones.add(str.substring(mid).toLong())
                }
                else -> nextStones.add(stone * 2024)
            }
        }
        stones = nextStones
    }
    println(stones.size)
}
