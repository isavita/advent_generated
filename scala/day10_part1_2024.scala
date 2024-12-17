
import scala.io.Source
import scala.collection.mutable

object Solution {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    val nr = lines.length
    val nc = lines(0).length
    val grid = lines.map(_.map(_ - '0'))
    val dirs = Array((1, 0), (-1, 0), (0, 1), (0, -1))
    val trailheads = for {
      r <- 0 until nr
      c <- 0 until nc
      if grid(r)(c) == 0
    } yield (r, c)

    var sumScores = 0
    for (th <- trailheads) {
      val reached = mutable.Set[(Int, Int)]()
      val front = mutable.Queue[((Int, Int), Int)]()
      front.enqueue((th, 0))
      val visited = mutable.Set[(Int, Int, Int)]()

      while (front.nonEmpty) {
        val (cur, h) = front.dequeue()
        if (h == 9) {
          reached.add(cur)
        } else {
          for ((dr, dc) <- dirs) {
            val (nr2, nc2) = (cur._1 + dr, cur._2 + dc)
            if (nr2 >= 0 && nr2 < nr && nc2 >= 0 && nc2 < nc && grid(nr2)(nc2) == h + 1) {
              val key = (nr2, nc2, h + 1)
              if (!visited.contains(key)) {
                visited.add(key)
                front.enqueue(((nr2, nc2), h + 1))
              }
            }
          }
        }
      }
      sumScores += reached.size
    }
    println(sumScores)
  }
}
