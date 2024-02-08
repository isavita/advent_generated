
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList.map(_.map(_.asDigit))

  def isLowPoint(i: Int, j: Int): Boolean = {
    val h = input(i)(j)
    val neighbors = List((i-1, j), (i+1, j), (i, j-1), (i, j+1))
    neighbors.forall { case (x, y) => x < 0 || x >= input.length || y < 0 || y >= input(0).length || h < input(x)(y) }
  }

  val lowPoints = for {
    i <- input.indices
    j <- input(0).indices
    if isLowPoint(i, j)
  } yield input(i)(j)

  val resultPart1 = lowPoints.map(_ + 1).sum

  def dfs(i: Int, j: Int, visited: Array[Array[Boolean]]): Int = {
    if (i < 0 || i >= input.length || j < 0 || j >= input(0).length || visited(i)(j) || input(i)(j) == 9) {
      0
    } else {
      visited(i)(j) = true
      1 + dfs(i-1, j, visited) + dfs(i+1, j, visited) + dfs(i, j-1, visited) + dfs(i, j+1, visited)
    }
  }

  val basins = scala.collection.mutable.ListBuffer.empty[Int]
  val visited = Array.ofDim[Boolean](input.length, input(0).length)
  for {
    i <- input.indices
    j <- input(0).indices
    if !visited(i)(j)
  } {
    basins += dfs(i, j, visited)
  }

  val resultPart2 = basins.sorted.takeRight(3).product

  println(resultPart1)
  println(resultPart2)
}
