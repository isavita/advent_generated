object Main extends App {
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.getLines().toList finally source.close()

  var totalCount = 0
  var groupAnswers = scala.collection.mutable.Map[Char, Int]()
  var groupSize = 0

  for (line <- lines) {
    if (line.isEmpty) {
      for ((_, count) <- groupAnswers) {
        if (count == groupSize) {
          totalCount += 1
        }
      }
      groupAnswers = scala.collection.mutable.Map[Char, Int]()
      groupSize = 0
    } else {
      groupSize += 1
      for (question <- line) {
        groupAnswers(question) = groupAnswers.getOrElse(question, 0) + 1
      }
    }
  }

  for ((_, count) <- groupAnswers) {
    if (count == groupSize) {
      totalCount += 1
    }
  }

  println(totalCount)
}