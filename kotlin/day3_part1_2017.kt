import java.io.File
import kotlin.math.abs
import kotlin.math.ceil
import kotlin.math.sqrt

fun main(args: Array<String>) {
    val data = File("input.txt").readText().trim().toInt()

    var target = data

    var sideLength = ceil(sqrt(target.toDouble())).toInt()
    if (sideLength % 2 == 0) {
        sideLength++
    }

    var maxValue = sideLength * sideLength
    var stepsFromEdge = (sideLength - 1) / 2
    var distanceToMiddle = 0

    for (i in 0 until 4) {
        var middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i
        var distance = abs(target - middlePoint)
        if (distance < distanceToMiddle || i == 0) {
            distanceToMiddle = distance
        }
    }

    var manhattanDistance = stepsFromEdge + distanceToMiddle

    println(manhattanDistance)
}