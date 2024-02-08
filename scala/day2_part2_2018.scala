
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  val countTwo = input.count(id => id.groupBy(identity).values.exists(_.size == 2))
  val countThree = input.count(id => id.groupBy(identity).values.exists(_.size == 3))

  println(countTwo * countThree)

  def findCommonLetters(ids: List[String]): String = {
    ids.combinations(2).foreach { case List(id1, id2) =>
      if (id1.zip(id2).count(pair => pair._1 != pair._2) == 1) {
        return id1.zip(id2).filter(pair => pair._1 == pair._2).map(_._1).mkString
      }
    }
    ""
  }

  println(findCommonLetters(input))
}
