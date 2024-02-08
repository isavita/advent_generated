
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList.map(_.map(_.asDigit))

  def isLowPoint(row: Int, col: Int): Boolean = {
    val height = input(row)(col)
    val neighbors = List(
      (row - 1, col),
      (row + 1, col),
      (row, col - 1),
      (row, col + 1)
    )

    neighbors.forall { case (r, c) =>
      if (r < 0 || r >= input.length || c < 0 || c >= input(0).length) true
      else height < input(r)(c)
    }
  }

  val lowPoints = for {
    row <- input.indices
    col <- input(0).indices
    if isLowPoint(row, col)
  } yield input(row)(col) + 1

  println(lowPoints.sum)
}
