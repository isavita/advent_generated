import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim().toInt()
    var target = input / 11

    val houses = IntArray(target + 1)

    for (elf in 1..target) {
        var house = elf
        while (house <= elf * 50 && house <= target) {
            houses[house] += elf
            house += elf
        }
    }

    var houseNumber = 0
    var presents = 0
    for (i in houses.indices) {
        if (houses[i] >= target) {
            houseNumber = i
            presents = houses[i]
            break
        }
    }

    println(houseNumber)
}