import java.io.File
import java.io.BufferedReader
import java.io.FileReader

fun main(args: Array<String>) {
    var maxCalories = 0
    var currentCalories = 0

    val file = File("input.txt")
    val bufferedReader = BufferedReader(FileReader(file))

    bufferedReader.useLines { lines ->
        lines.forEach {
            if (it.isEmpty()) {
                if (currentCalories > maxCalories) {
                    maxCalories = currentCalories
                }
                currentCalories = 0
            } else {
                currentCalories += it.toInt()
            }
        }
    }

    if (currentCalories > maxCalories) {
        maxCalories = currentCalories
    }

    println(maxCalories)
}