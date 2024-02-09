import java.io.File

fun main(args: Array<String>) {
    val positions = mutableListOf<Int>()
    File("input.txt").forEachLine {
        val numbers = it.split(",")
        numbers.forEach { num_str ->
            positions.add(num_str.toInt())
        }
    }

    positions.sort()

    var min_fuel = Int.MAX_VALUE
    for (i in positions[0]..positions[positions.size - 1]) {
        var fuel = 0
        positions.forEach { pos ->
            fuel += calculateNewFuel(pos, i)
        }
        if (fuel < min_fuel) {
            min_fuel = fuel
        }
    }
    println(min_fuel)
}

fun calculateNewFuel(currentPosition: Int, newPosition: Int): Int {
    val diff = abs(currentPosition - newPosition)
    return (diff * (diff + 1)) / 2
}

fun abs(n: Int): Int {
    return if (n < 0) -n else n
}