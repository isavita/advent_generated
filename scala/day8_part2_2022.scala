import scala.io.Source

object Day8 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    val grid = lines.map(_.toCharArray)

    // Part 1: Count visible trees
    val visibleTrees = countVisibleTrees(grid)
    println(s"Part 1: $visibleTrees trees are visible from outside the grid")

    // Part 2: Find the highest scenic score
    val highestScenicScore = findHighestScenicScore(grid)
    println(s"Part 2: The highest scenic score is $highestScenicScore")
  }

  def countVisibleTrees(grid: Array[Array[Char]]): Int = {
    var visibleTrees = 0
    for (i <- grid.indices; j <- grid(i).indices) {
      if (isVisible(grid, i, j)) {
        visibleTrees += 1
      }
    }
    visibleTrees
  }

  def isVisible(grid: Array[Array[Char]], i: Int, j: Int): Boolean = {
    val height = grid(i)(j)
    val (up, down, left, right) = (isVisibleFrom(grid, i, j, -1, 0), isVisibleFrom(grid, i, j, 1, 0),
      isVisibleFrom(grid, i, j, 0, -1), isVisibleFrom(grid, i, j, 0, 1))
    up || down || left || right
  }

  def isVisibleFrom(grid: Array[Array[Char]], i: Int, j: Int, di: Int, dj: Int): Boolean = {
    var (ii, jj) = (i + di, j + dj)
    while (ii >= 0 && jj >= 0 && ii < grid.length && jj < grid(ii).length) {
      if (grid(ii)(jj) >= grid(i)(j)) {
        return false
      }
      ii += di
      jj += dj
    }
    true
  }

  def findHighestScenicScore(grid: Array[Array[Char]]): Int = {
    var highestScore = 0
    for (i <- grid.indices; j <- grid(i).indices) {
      val score = scenicScore(grid, i, j)
      highestScore = math.max(highestScore, score)
    }
    highestScore
  }

  def scenicScore(grid: Array[Array[Char]], i: Int, j: Int): Int = {
    val height = grid(i)(j)
    val (up, down, left, right) = (viewingDistance(grid, i, j, -1, 0), viewingDistance(grid, i, j, 1, 0),
      viewingDistance(grid, i, j, 0, -1), viewingDistance(grid, i, j, 0, 1))
    up * down * left * right
  }

  def viewingDistance(grid: Array[Array[Char]], i: Int, j: Int, di: Int, dj: Int): Int = {
    var (ii, jj) = (i + di, j + dj)
    var distance = 0
    while (ii >= 0 && jj >= 0 && ii < grid.length && jj < grid(ii).length) {
      distance += 1
      if (grid(ii)(jj) >= grid(i)(j)) {
        return distance
      }
      ii += di
      jj += dj
    }
    distance
  }
}