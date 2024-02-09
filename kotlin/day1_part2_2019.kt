import java.io.File

fun calculateFuel(mass: Int): Int = mass / 3 - 2

fun calculateTotalFuel(mass: Int): Int {
    var totalFuel = 0
    var fuel = calculateFuel(mass)
    while (fuel > 0) {
        totalFuel += fuel
        fuel = calculateFuel(fuel)
    }
    return totalFuel
}

fun main() {
    var totalFuel = 0
    File("input.txt").forEachLine {
        totalFuel += calculateTotalFuel(it.toInt())
    }
    println(totalFuel)
}