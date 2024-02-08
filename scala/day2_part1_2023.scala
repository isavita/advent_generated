object Solution extends App {
  import scala.io.Source
  import scala.util.matching.Regex

  val file = Source.fromFile("input.txt")
  val lines = file.getLines().toList
  file.close()

  val gameRegex: Regex = "Game (\\d+): (.+)".r
  val cubeRegex: Regex = "(\\d+) (red|green|blue)".r

  var totalSum = 0

  for (line <- lines) {
    line match {
      case gameRegex(gameId, rounds) =>
        var isValid = true
        val roundList = rounds.split(";")

        for (round <- roundList) {
          val cubes = cubeRegex.findAllIn(round).toList
          var red = 0
          var green = 0
          var blue = 0

          for (cube <- cubes) {
            val Array(count, color) = cube.split(" ")
            color match {
              case "red" => red += count.toInt
              case "green" => green += count.toInt
              case "blue" => blue += count.toInt
            }

            if (red > 12 || green > 13 || blue > 14) {
              isValid = false
            }
          }

          if (!isValid) {
            isValid = false
          }
        }

        if (isValid) {
          totalSum += gameId.toInt
        }

      case _ =>
    }
  }

  println(totalSum)
}