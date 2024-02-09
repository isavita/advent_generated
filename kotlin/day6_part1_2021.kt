import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()
    val fishStrs = lines[0].split(",")
    val fishes = IntArray(9)

    fishStrs.forEach {
        val fish = it.toInt()
        fishes[fish]++
    }

    for (day in 1..80) {
        val newFish = fishes[0]
        for (i in 1 until fishes.size) {
            fishes[i - 1] = fishes[i]
        }
        fishes[6] += newFish
        fishes[8] = newFish
    }

    var totalFish = 0
    fishes.forEach {
        totalFish += it
    }

    println(totalFish)
}