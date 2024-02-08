
object Day16AuntSue {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().toList
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

    val sueList = input.map { line =>
      val sueRegex = """Sue (\d+): (.*)""".r
      val sueRegex(sueNum, properties) = line
      val sueProps = properties.split(", ").map(_.split(": ")).map(arr => (arr(0), arr(1).toInt)).toMap
      (sueNum.toInt, sueProps)
    }

    val result = sueList.find { case (sueNum, sueProps) =>
      sueProps.forall { case (prop, value) =>
        mfcsam(prop) == value
      }
    }.map(_._1)

    println(result.get)
  }
}
