
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    val totalCalibrationResult = solve(lines)
    println(totalCalibrationResult)

    val totalCalibrationResultPart2 = solvePart2(lines)
    println(totalCalibrationResultPart2)
}

fun solve(lines: List<String>): Long {
    return lines.sumOf { line ->
        val (targetStr, numbersStr) = line.split(":")
        val target = targetStr.trim().toLong()
        val numbers = numbersStr.trim().split(" ").map { it.toLong() }

        if (canReachTarget(target, numbers, setOf("+", "*"))) {
            target
        } else {
            0
        }
    }
}

fun solvePart2(lines: List<String>): Long {
    return lines.sumOf { line ->
        val (targetStr, numbersStr) = line.split(":")
        val target = targetStr.trim().toLong()
        val numbers = numbersStr.trim().split(" ").map { it.toLong() }

        if (canReachTarget(target, numbers, setOf("+", "*", "||"))) {
            target
        } else {
            0
        }
    }
}
fun canReachTarget(target: Long, numbers: List<Long>, operators: Set<String>): Boolean {
    if (numbers.size == 1) {
        return numbers[0] == target
    }

    val results = mutableSetOf<Long>()

    fun calculate(currentIndex: Int, currentResult: Long) {
        if (currentIndex == numbers.size) {
            results.add(currentResult)
            return
        }

        if (currentIndex == 0) {
            calculate(currentIndex + 1, numbers[currentIndex])
        } else {
            for (op in operators) {
                when (op) {
                    "+" -> calculate(currentIndex + 1, currentResult + numbers[currentIndex])
                    "*" -> calculate(currentIndex + 1, currentResult * numbers[currentIndex])
                    "||" -> {
                        val concatenated = "$currentResult${numbers[currentIndex]}".toLongOrNull()
                        if (concatenated != null) {
                            calculate(currentIndex + 1, concatenated)
                        }
                    }
                }
            }
        }
    }
    calculate(0,0)

    return results.contains(target)
}
