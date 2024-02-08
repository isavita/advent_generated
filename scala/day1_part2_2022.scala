object Solution extends App {
    import scala.io.Source

    val filename = "input.txt"
    var caloriesList = List[Int]()
    var currentCalories = 0

    for (line <- Source.fromFile(filename).getLines) {
        if (line == "") {
            caloriesList = caloriesList :+ currentCalories
            currentCalories = 0
        } else {
            currentCalories += line.toInt
        }
    }

    caloriesList = caloriesList :+ currentCalories
    val sortedCaloriesList = caloriesList.sorted.reverse

    var topThreeSum = 0
    for (i <- 0 until 3.min(sortedCaloriesList.length)) {
        topThreeSum += sortedCaloriesList(i)
    }

    println(topThreeSum)
}