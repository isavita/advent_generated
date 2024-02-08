object Solution extends App {
  import scala.io.Source

  case class Coordinate4D(x: Int, y: Int, z: Int, w: Int)

  val input = Source.fromFile("input.txt").getLines.toArray
  val initialState = input.map(_.trim)

  var activeCubes = scala.collection.mutable.Map[Coordinate4D, Boolean]()

  for {
    (line, y) <- initialState.zipWithIndex
    (char, x) <- line.zipWithIndex
    if char == '#'
  } activeCubes += (Coordinate4D(x, y, 0, 0) -> true)

  for (_ <- 0 until 6) {
    activeCubes = simulateCycle4D(activeCubes)
  }

  println(activeCubes.size)

  def simulateCycle4D(activeCubes: scala.collection.mutable.Map[Coordinate4D, Boolean]): scala.collection.mutable.Map[Coordinate4D, Boolean] = {
    val newActiveCubes = scala.collection.mutable.Map[Coordinate4D, Boolean]()
    val neighborCounts = scala.collection.mutable.Map[Coordinate4D, Int]()

    for {
      coord <- activeCubes.keys
      dw <- -1 to 1
      dz <- -1 to 1
      dy <- -1 to 1
      dx <- -1 to 1
      if dw != 0 || dz != 0 || dy != 0 || dx != 0
    } {
      val neighbor = Coordinate4D(coord.x + dx, coord.y + dy, coord.z + dz, coord.w + dw)
      neighborCounts(neighbor) = neighborCounts.getOrElse(neighbor, 0) + 1
    }

    for ((coord, count) <- neighborCounts) {
      if (count == 3 || (count == 2 && activeCubes.getOrElse(coord, false))) {
        newActiveCubes += (coord -> true)
      }
    }

    newActiveCubes
  }
}