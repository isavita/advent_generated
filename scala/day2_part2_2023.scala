
import scala.io.Source
import scala.util.matching.Regex

object Main extends App {
  val file = Source.fromFile("input.txt")
  val regex = """Game (\d+): (.+)""".r
  val cubeRegex = """(\d+) (red|green|blue)""".r
  var totalPower = 0

  for (line <- file.getLines()) {
    line match {
      case regex(_, rounds) =>
        var maxRed = 0
        var maxGreen = 0
        var maxBlue = 0

        for (round <- rounds.split(";")) {
          var red = 0
          var green = 0
          var blue = 0

          for (cube <- cubeRegex.findAllMatchIn(round)) {
            val count = cube.group(1).toInt
            cube.group(2) match {
              case "red" => red += count
              case "green" => green += count
              case "blue" => blue += count
            }
          }

          if (red > maxRed) maxRed = red
          if (green > maxGreen) maxGreen = green
          if (blue > maxBlue) maxBlue = blue
        }

        val power = maxRed * maxGreen * maxBlue
        totalPower += power
      case _ =>
    }
  }

  file.close()
  println(totalPower)
}
