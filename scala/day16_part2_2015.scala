
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  val suePattern = "Sue (\\d+): (.*)".r
  val mfcsam = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1
  )

  def parseLine(line: String): (Int, Map[String, Int]) = {
    val suePattern(number, properties) = line
    val sueProperties = properties.split(", ").map(_.split(": ")).map(arr => (arr(0), arr(1).toInt)).toMap
    (number.toInt, sueProperties)
  }

  val sues = input.map(parseLine)

  val realSue = sues.find { case (number, properties) =>
    properties.forall { case (property, value) =>
      property match {
        case "cats" | "trees" => value > mfcsam(property)
        case "pomeranians" | "goldfish" => value < mfcsam(property)
        case _ => value == mfcsam(property)
      }
    }
  }

  println(realSue.get._1)
}
