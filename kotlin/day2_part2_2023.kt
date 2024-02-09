import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val regex = Regex("""Game (\d+): (.+)""")
    val cubeRegex = Regex("""(\d+) (red|green|blue)""")
    var totalPower = 0

    file.forEachLine {
        val matches = regex.find(it)

        if (matches != null) {
            val rounds = matches.groupValues[2].split(";")
            var maxRed = 0
            var maxGreen = 0
            var maxBlue = 0

            rounds.forEach { round ->
                val cubes = cubeRegex.findAll(round)
                var red = 0
                var green = 0
                var blue = 0

                cubes.forEach { cube ->
                    val count = cube.groupValues[1].toInt()
                    when (cube.groupValues[2]) {
                        "red" -> red += count
                        "green" -> green += count
                        "blue" -> blue += count
                    }
                }

                if (red > maxRed) {
                    maxRed = red
                }
                if (green > maxGreen) {
                    maxGreen = green
                }
                if (blue > maxBlue) {
                    maxBlue = blue
                }
            }

            val power = maxRed * maxGreen * maxBlue
            totalPower += power
        }
    }

    println(totalPower)
}