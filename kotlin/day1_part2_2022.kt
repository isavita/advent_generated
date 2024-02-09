import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    var caloriesList = mutableListOf<Int>()
    var currentCalories = 0

    for (line in lines) {
        if (line.isEmpty()) {
            caloriesList.add(currentCalories)
            currentCalories = 0
            continue
        }

        val calories = line.toInt()
        currentCalories += calories
    }

    caloriesList.add(currentCalories)
    caloriesList.sortDescending()

    var topThreeSum = 0
    for (i in 0 until 3.coerceAtMost(caloriesList.size)) {
        topThreeSum += caloriesList[i]
    }

    println(topThreeSum)
}