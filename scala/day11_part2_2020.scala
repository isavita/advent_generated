
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  def countOccupiedSeats(seats: List[String], x: Int, y: Int): Int = {
    val directions = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
    directions.map { case (dx, dy) =>
      LazyList.from(1)
        .map(i => (x + i * dx, y + i * dy))
        .takeWhile { case (nx, ny) => nx >= 0 && nx < seats.length && ny >= 0 && ny < seats.head.length }
        .map { case (nx, ny) => seats(nx)(ny) }
        .find(_ != '.')
    }.count(_ == Some('#'))
  }

  def simulateSeating(seats: List[String]): List[String] = {
    seats.zipWithIndex.map { case (row, x) =>
      row.zipWithIndex.map { case (seat, y) =>
        seat match {
          case 'L' if countOccupiedSeats(seats, x, y) == 0 => '#'
          case '#' if countOccupiedSeats(seats, x, y) >= 5 => 'L'
          case other => other
        }
      }.mkString
    }
  }

  def findEquilibrium(seats: List[String]): List[String] = {
    val nextSeats = simulateSeating(seats)
    if (seats == nextSeats) seats
    else findEquilibrium(nextSeats)
  }

  val finalSeats = findEquilibrium(input)
  val occupiedSeats = finalSeats.map(_.count(_ == '#')).sum
  println(occupiedSeats)
}
