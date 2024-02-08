object Solution extends App {
  import java.io.File
  import scala.io.Source

  val Neighbors4 = List((0, 1), (0, -1), (1, 0), (-1, 0))

  val grid = scala.collection.mutable.Map[(Int, Int), Int]()
  val visible = scala.collection.mutable.Set[(Int, Int)]()

  val bufferedSource = Source.fromFile("input.txt")
  var y = 0

  for (line <- bufferedSource.getLines) {
    line.zipWithIndex.foreach { case (b, x) =>
      grid((x, y)) = b.toInt - '0'
    }
    y += 1
  }
  bufferedSource.close()

  for ((p, _) <- grid) {
    for (n <- Neighbors4) {
      var next = p
      var addNext = true
      while (addNext) {
        next = (next._1 + n._1, next._2 + n._2)
        if (grid.contains(next)) {
          if (grid(next) >= grid(p)) {
            addNext = false
          }
        } else {
          visible.add(p)
          addNext = false
        }
      }
    }
  }
  println(visible.size)
}