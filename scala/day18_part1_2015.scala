object Day18 {
  def main(args: Array[String]): Unit = {
    val input = io.Source.fromFile("input.txt").getLines().toArray
    var grid = input.map(_.toCharArray)
    for (_ <- 1 to 100) {
      grid = step(grid)
    }
    val lightsOn = grid.flatten.count(_ == '#')
    println(s"After 100 steps, there are $lightsOn lights on.")
  }

  def step(grid: Array[Array[Char]]): Array[Array[Char]] = {
    val rows = grid.length
    val cols = grid(0).length
    val newGrid = Array.ofDim[Char](rows, cols)
    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      val neighbors = countNeighbors(grid, i, j)
      if (grid(i)(j) == '#') {
        newGrid(i)(j) = if (neighbors == 2 || neighbors == 3) '#' else '.'
      } else {
        newGrid(i)(j) = if (neighbors == 3) '#' else '.'
      }
    }
    newGrid
  }

  def countNeighbors(grid: Array[Array[Char]], i: Int, j: Int): Int = {
    val rows = grid.length
    val cols = grid(0).length
    var count = 0
    for {
      x <- -1 to 1
      y <- -1 to 1
      if (x != 0 || y != 0)
      ii = i + x
      jj = j + y
      if ii >= 0 && ii < rows && jj >= 0 && jj < cols
    } {
      if (grid(ii)(jj) == '#') count += 1
    }
    count
  }
}