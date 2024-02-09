import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim().toInt()
    var target = input / 10
    val houses = IntArray(target + 1)

    for (elf in 1..target) {
        for (house in elf..target step elf) {
            houses[house] += elf
        }
    }

    var houseNumber = 0
    for ((index, presents) in houses.withIndex()) {
        if (presents >= target) {
            houseNumber = index
            break
        }
    }

    println(houseNumber)
}