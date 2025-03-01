
import java.io.File
import java.io.IOException

fun main() {
    try {
        val totalCalibrationResult = File("input.txt").readLines().sumOf { line ->
            val (targetStr, numbersStr) = line.split(":")
            val target = targetStr.trim().toLong()
            val numbers = numbersStr.trim().split(" ").map { it.toLong() }

            if (canReachTarget(target, numbers)) target else 0L
        }
        println(totalCalibrationResult)

    } catch (e: IOException) {
        println("Error reading file: ${e.message}")
    }
}

fun canReachTarget(target: Long, numbers: List<Long>): Boolean {
    if (numbers.isEmpty()) {
        return false
    }

    fun solve(index: Int, currentResult: Long): Boolean {
        if (index == numbers.size) {
            return currentResult == target
        }

        if (index == 0) {
            return solve(index + 1, numbers[index])
        }

        return solve(index + 1, currentResult + numbers[index]) ||
               solve(index + 1, currentResult * numbers[index])
    }
    
    return solve(0, 0)
}
