
import scala.io.Source

object Main {
  val Side = 5
  val Square = Side * Side

  def parse(): Array[Boolean] = {
    val source = Source.fromFile("input.txt")
    val res = Array.fill(Square)(false)
    for ((line, row) <- source.getLines().zipWithIndex) {
      for (col <- 0 until Side) {
        res(row * Side + col) = line(col) == '#'
      }
    }
    source.close()
    res
  }

  type Space = Map[Int, Array[Boolean]]

  def main(args: Array[String]): Unit = {
    var space = Map(0 -> parse())
    for (_ <- 0 until 200) {
      space = next(space)
    }
    val count = space.values.flatten.count(identity)
    println(count)
  }

  def next(space: Space): Space = {
    val newSpace = scala.collection.mutable.Map[Int, Array[Boolean]]()
    val (minLevel, maxLevel) = minMaxLevel(space)

    for (level <- minLevel - 1 to maxLevel + 1) {
      val newGrid = Array.fill(Square)(false)
      for (cell <- 0 until Square if cell != 12) {
        val neighbours = countNeighbours(space, level, cell)
        newGrid(cell) = (infested(space, level, cell) && neighbours == 1) || 
                        (!infested(space, level, cell) && (neighbours == 1 || neighbours == 2))
      }
      clean(newSpace, level, newGrid)
    }
    newSpace.toMap
  }

  def countNeighbours(space: Space, level: Int, cell: Int): Int = {
    var neighbours = 0
    val (row, col) = (cell / Side, cell % Side)

    if (row == 0 && infested(space, level - 1, 7)) neighbours += 1
    if (col == 0 && infested(space, level - 1, 11)) neighbours += 1
    if (col == 4 && infested(space, level - 1, 13)) neighbours += 1
    if (row == 4 && infested(space, level - 1, 17)) neighbours += 1
    if (cell == 7) for (i <- 0 until Side if infested(space, level + 1, i)) neighbours += 1
    if (cell == 11) for (i <- 0 until Side if infested(space, level + 1, 5 * i)) neighbours += 1
    if (cell == 13) for (i <- 0 until Side if infested(space, level + 1, 5 * i + Side - 1)) neighbours += 1
    if (cell == 17) for (i <- 0 until Side if infested(space, level + 1, (Side - 1) * Side + i)) neighbours += 1

    if (row > 0 && cell != 17 && infested(space, level, cell - Side)) neighbours += 1
    if (col > 0 && cell != 13 && infested(space, level, cell - 1)) neighbours += 1
    if (col < Side - 1 && cell != 11 && infested(space, level, cell + 1)) neighbours += 1
    if (row < Side - 1 && cell != 7 && infested(space, level, cell + Side)) neighbours += 1

    neighbours
  }

  def clean(newSpace: scala.collection.mutable.Map[Int, Array[Boolean]], level: Int, newGrid: Array[Boolean]): Unit = {
    newSpace(level) = newGrid
    if (newGrid.forall(!_)) newSpace -= level
  }

  def infested(space: Space, level: Int, cell: Int): Boolean = {
    space.get(level).exists(_(cell))
  }

  def minMaxLevel(space: Space): (Int, Int) = {
    val levels = space.keys.toList
    (levels.min, levels.max)
  }
}
