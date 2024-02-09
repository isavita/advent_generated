import java.io.File

fun main(args: Array<String>) {
    val regex = Regex("""Game (\d+): (.+)""")
    val cubeRegex = Regex("""(\d+) (red|green|blue)""")
    var totalSum = 0

    File("input.txt").forEachLine {
        val matches = regex.find(it)

        if (matches != null) {
            val gameId = matches.groupValues[1].toInt()
            val rounds = matches.groupValues[2].split(";")
            var isValid = true

            for (round in rounds) {
                val cubes = cubeRegex.findAll(round)
                var red = 0
                var green = 0
                var blue = 0

                for (cube in cubes) {
                    val count = cube.groupValues[1].toInt()
                    when (cube.groupValues[2]) {
                        "red" -> red += count
                        "green" -> green += count
                        "blue" -> blue += count
                    }

                    if (red > 12 || green > 13 || blue > 14) {
                        isValid = false
                        break
                    }
                }

                if (!isValid) {
                    break
                }
            }

            if (isValid) {
                totalSum += gameId
            }
        }
    }

    println(totalSum)
}