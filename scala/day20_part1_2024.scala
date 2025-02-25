
import scala.io.Source
import scala.collection.mutable

object Solution {

  def solve(): Unit = {
    val grid = Source.fromFile("input.txt").getLines().toArray
    val h = grid.length
    val w = grid(0).length

    var S = (-1, -1)
    var E = (-1, -1)

    for (r <- 0 until h; c <- 0 until w) {
      grid(r)(c) match {
        case 'S' => S = (r, c)
        case 'E' => E = (r, c)
        case _   =>
      }
    }

    val trackCells = mutable.ArrayBuffer[(Int, Int)]()
    val walls = Array.fill(h, w)(false)
    for (r <- 0 until h; c <- 0 until w) {
      if (grid(r)(c) == '#') {
        walls(r)(c) = true
      } else {
        trackCells.append((r, c))
      }
    }

    val dirs = Array((1, 0), (-1, 0), (0, 1), (0, -1))

    def bfs(start: (Int, Int), ignoreWalls: Boolean = false): Array[Array[Int]] = {
      val dist = Array.fill(h, w)(-1)
      dist(start._1)(start._2) = 0
      val q = mutable.Queue(start)

      while (q.nonEmpty) {
        val (r, c) = q.dequeue()
        for ((dr, dc) <- dirs) {
          val (nr, nc) = (r + dr, c + dc)
          if (0 <= nr && nr < h && 0 <= nc && nc < w) {
            if (!ignoreWalls && walls(nr)(nc)) {} else {
              if (dist(nr)(nc) == -1) {
                dist(nr)(nc) = dist(r)(c) + 1
                q.enqueue((nr, nc))
              }
            }
          }
        }
      }
      dist
    }

    val distFromS = bfs(S)
    val distFromE = bfs(E)

    if (distFromS(E._1)(E._2) == -1) {
      println(0)
      return
    }

    val normalCost = distFromS(E._1)(E._2)

    def isTrack(r: Int, c: Int): Boolean = {
      0 <= r && r < h && 0 <= c && c < w && !walls(r)(c)
    }

    var possibleCheats = 0
    for (startPos <- trackCells) {
      val sd = distFromS(startPos._1)(startPos._2)
      if (sd != -1) {
        for ((dr1, dc1) <- dirs) {
          val (m1r, m1c) = (startPos._1 + dr1, startPos._2 + dc1)
          if (0 <= m1r && m1r < h && 0 <= m1c && m1c < w) {
            for ((dr2, dc2) <- dirs) {
              val (m2r, m2c) = (m1r + dr2, m1c + dc2)
              if (0 <= m2r && m2r < h && 0 <= m2c && m2c < w && isTrack(m2r, m2c)) {
                val ed = distFromE(m2r)(m2c)
                if (ed != -1) {
                  val newCost = sd + 2 + ed
                  val saving = normalCost - newCost
                  if (saving >= 100) {
                    possibleCheats += 1
                  }
                }
              }
            }
          }
        }
      }
    }
    println(possibleCheats)
  }

  def main(args: Array[String]): Unit = {
    solve()
  }
}
