
import scala.io.Source
import scala.collection.mutable

object Solution {

  case class P(x: Int, y: Int) {
    def neighbours: Seq[P] = Seq(
      P(x, y + 1),
      P(x + 1, y),
      P(x, y - 1),
      P(x - 1, y)
    )
  }

  case class Status(p: P, depth: Int)

  def main(args: Array[String]): Unit = {
    val (grid, xMax, yMax, aa, zz, teleport, portalName, isOuter) = parse("input.txt")
    println(bfsNested(grid, xMax, yMax, aa, zz, teleport, portalName, isOuter))
  }

  def parse(filename: String): (Map[P, Char], Int, Int, P, P, Map[P, P], Map[P, String], Map[P, Boolean]) = {
    val lines = Source.fromFile(filename).getLines().toArray
    val xMax = lines.length
    val yMax = lines.map(_.length).max
    val grid = mutable.Map.empty[P, Char]

    for (i <- 0 until xMax; j <- 0 until yMax) {
      if (j < lines(i).length) {
        grid(P(i, j)) = lines(i)(j)
      }
    }

    var aa = P(0, 0)
    var zz = P(0, 0)
    val isOuter = mutable.Map.empty[P, Boolean]
    val portalName = mutable.Map.empty[P, String]
    val teleport = mutable.Map.empty[P, P]
    val cache = mutable.Map.empty[String, P]

    for (i <- 0 until xMax; j <- 0 until yMax) {
      val c = grid.getOrElse(P(i, j), ' ')
      if (c.isLetter) {
        extractPortal(grid.toMap, P(i, j)) match {
          case (pName, pPoint, true) =>
            portalName(pPoint) = pName
            if (pName == "AA") {
              aa = pPoint
              isOuter(pPoint) = true
            } else if (pName == "ZZ") {
              zz = pPoint
              isOuter(pPoint) = true
            } else {
              cache.get(pName) match {
                case Some(target) =>
                  teleport(pPoint) = target
                  teleport(target) = pPoint
                case None =>
                  cache(pName) = pPoint
              }
              isOuter(pPoint) = (j == 0 || i == 0 || i == xMax - 2 || j == yMax - 2)
            }
          case _ =>
        }
      }
    }

    (grid.toMap, xMax, yMax, aa, zz, teleport.toMap, portalName.toMap, isOuter.toMap)
  }

  def extractPortal(grid: Map[P, Char], p: P): (String, P, Boolean) = {
    val c1 = grid.getOrElse(p, ' ')
    if (grid.getOrElse(P(p.x + 1, p.y), ' ').isLetter) {
      val portalName = s"$c1${grid(P(p.x + 1, p.y))}"
      val portalPoint1 = P(p.x + 2, p.y)
      if (grid.getOrElse(portalPoint1, ' ') == '.') {
        return (portalName, portalPoint1, true)
      }
      val portalPoint2 = P(p.x - 1, p.y)
      if (grid.getOrElse(portalPoint2, ' ') == '.') {
        return (portalName, portalPoint2, true)
      }
    }
    if (grid.getOrElse(P(p.x, p.y + 1), ' ').isLetter) {
      val portalName = s"$c1${grid(P(p.x, p.y + 1))}"
      val portalPoint1 = P(p.x, p.y + 2)
      if (grid.getOrElse(portalPoint1, ' ') == '.') {
        return (portalName, portalPoint1, true)
      }
      val portalPoint2 = P(p.x, p.y - 1)
      if (grid.getOrElse(portalPoint2, ' ') == '.') {
        return (portalName, portalPoint2, true)
      }
    }
    ("", P(0, 0), false)
  }

  def bfsNested(grid: Map[P, Char], xMax: Int, yMax: Int, aa: P, zz: P, teleport: Map[P, P], portalName: Map[P, String], isOuter: Map[P, Boolean]): Int = {
    val discovered = mutable.Set.empty[Status]
    val toDo = mutable.Queue.empty[Status]
    val root = Status(aa, 0)
    discovered += root
    toDo.enqueue(root)
    var steps = 0

    while (toDo.nonEmpty) {
      val levelSize = toDo.size
      for (_ <- 0 until levelSize) {
        val curr = toDo.dequeue()
        for (n <- curr.p.neighbours) {
          grid.get(n) match {
            case Some('#') =>
            case Some('.') =>
              val target = Status(n, curr.depth)
              if (!discovered.contains(target)) {
                discovered += target
                toDo.enqueue(target)
              }
            case Some(c) if c.isLetter =>
              val outer = isOuter(curr.p)
              val target = if (!outer) {
                Status(teleport(curr.p), curr.depth + 1)
              } else {
                val pName = portalName(curr.p)
                if (curr.depth == 0) {
                  if (pName == "ZZ") {
                    return steps
                  } else {
                    Status(P(-1, -1), -1)
                  }
                } else if (pName == "AA" || pName == "ZZ") {
                  Status(P(-1, -1), -1)
                } else {
                  Status(teleport(curr.p), curr.depth - 1)
                }
              }

              if (target.p.x != -1 && !discovered.contains(target)) {
                discovered += target
                toDo.enqueue(target)
              }
            case _ =>
          }
        }
      }
      steps += 1
    }
    -1
  }
}
