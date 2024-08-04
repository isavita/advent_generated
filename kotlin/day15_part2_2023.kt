import java.io.File

fun main() {
    val input = File("input.txt").readText().trim()
    val steps = input.split(",")

    // Part One
    val partOneResult = steps.sumOf { step -> hash(step) }
    println("Part One: $partOneResult")

    // Part Two
    val boxes = List(256) { mutableListOf<Pair<String, Int>>() }

    steps.forEach { step ->
        val label = step.takeWhile { it.isLetter() }
        val operation = step[label.length]
        val boxNumber = hash(label)

        when (operation) {
            '=' -> {
                val focalLength = step.substring(label.length + 1).toInt()
                val box = boxes[boxNumber]
                val lensIndex = box.indexOfFirst { it.first == label }
                if (lensIndex != -1) {
                    box[lensIndex] = label to focalLength
                } else {
                    box.add(label to focalLength)
                }
            }
            '-' -> {
                val box = boxes[boxNumber]
                box.removeIf { it.first == label }
            }
        }
    }

    val partTwoResult = boxes.flatMapIndexed { boxIndex, lenses ->
        lenses.mapIndexed { lensIndex, (_, focalLength) ->
            (boxIndex + 1) * (lensIndex + 1) * focalLength
        }
    }.sum()

    println("Part Two: $partTwoResult")
}

fun hash(input: String): Int {
    var currentValue = 0
    for (char in input) {
        currentValue += char.code
        currentValue *= 17
        currentValue %= 256
    }
    return currentValue
}