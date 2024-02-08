object Main extends App {
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.getLines().toList finally source.close()

  val inputData = lines.flatMap(_.split(",")).map(_.toInt).toArray

  inputData(1) = 12
  inputData(2) = 2

  val result = executeProgram(inputData)

  println(result)

  def executeProgram(data: Array[Int]): Int = {
    for (i <- data.indices by 4) {
      data(i) match {
        case 1 =>
          val sum = data(data(i + 1)) + data(data(i + 2))
          data(data(i + 3)) = sum
        case 2 =>
          val product = data(data(i + 1)) * data(data(i + 2))
          data(data(i + 3)) = product
        case 99 => return data(0)
        case _ => throw new IllegalArgumentException("Invalid opcode")
      }
    }

    data(0)
  }
}