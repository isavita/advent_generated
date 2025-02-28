import scala.collection.mutable
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val grid = Source.fromFile("input.txt").getLines().toArray.map(_.trim)
    val h = grid.length
    val w = grid(0).length
    val walls = Array.ofDim[Boolean](h, w)
    var start: (Int, Int) = (0, 0)
    var end: (Int, Int) = (0, 0)
    val trackCells = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until h; j <- 0 until w) {
      grid(i)(j) match {
        case 'S' => start = (i, j)
        case 'E' => end = (i, j)
        case '#' => walls(i)(j) = true
        case _   =>
      }
      if (grid(i)(j) != '#') trackCells += ((i, j))
    }

    val dirs = Array((1, 0), (-1, 0), (0, 1), (0, -1))
    def bfs(src: (Int, Int)): Array[Array[Int]] = {
      val dist = Array.fill(h, w)(-1)
      val q = mutable.Queue[(Int, Int)]()
      val (si, sj) = src
      dist(si)(sj) = 0
      q.enqueue(src)
      while (q.nonEmpty) {
        val (i, j) = q.dequeue()
        for ((dx, dy) <- dirs) {
          val ni = i + dx
          val nj = j + dy
          if (ni >= 0 && ni < h && nj >= 0 && nj < w && !walls(ni)(nj) && dist(ni)(nj) == -1) {
            dist(ni)(nj) = dist(i)(j) + 1
            q.enqueue((ni, nj))
          }
        }
      }
      dist
    }

    val distFromS = bfs(start)
    val distFromE = bfs(end)
    if (distFromS(end._1)(end._2) == -1) {
      println(0)
      return
    }
    val normalCost = distFromS(end._1)(end._2)
    val cheats = mutable.Map[(Int, Int, Int, Int), Int]()
    
    for ((si, sj) <- trackCells) {
      val sd = distFromS(si)(sj)
      if (sd != -1) {
        val distC = Array.fill(h, w)(-1)
        val q = mutable.Queue[(Int, Int)]()
        distC(si)(sj) = 0
        q.enqueue((si, sj))
        while (q.nonEmpty) {
          val (i, j) = q.dequeue()
          if (distC(i)(j) < 20)
            for ((dx, dy) <- dirs) {
              val ni = i + dx
              val nj = j + dy
              if (ni >= 0 && ni < h && nj >= 0 && nj < w && distC(ni)(nj) == -1) {
                distC(ni)(nj) = distC(i)(j) + 1
                q.enqueue((ni, nj))
              }
            }
        }
        for (i <- 0 until h; j <- 0 until w) {
          val s = distC(i)(j)
          if (s > 0 && s <= 20 && !walls(i)(j)) {
            val ed = distFromE(i)(j)
            if (ed != -1) {
              val cost = sd + s + ed
              if (cost < normalCost) {
                val key = (si, sj, i, j)
                cheats(key) = math.min(cheats.getOrElse(key, cost), cost)
              }
            }
          }
        }
      }
    }
    val count = cheats.values.count(cost => normalCost - cost >= 100)
    println(count)
  }
}