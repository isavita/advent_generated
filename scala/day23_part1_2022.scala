
import scala.io.Source
import scala.collection.mutable

object Main {
  type P = (Int, Int)
  type Elf = (P, Boolean, P)

  val DIRS = Array(
    (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)
  )
  val ORDER = Array(1, 5, 7, 3)

  def aroundAllEmpty(elf: Elf, map: Set[P]): Boolean = {
    val (pos, _, _) = elf
    DIRS.forall { case (dx, dy) => !map.contains((pos._1 + dx, pos._2 + dy)) }
  }

  def elfInDirection(elf: Elf, map: Set[P], dir: Int): Boolean = {
    val (pos, _, _) = elf
    (-1 to 1).exists { j =>
      val (dx, dy) = DIRS((dir + j + 8) % 8)
      map.contains((pos._1 + dx, pos._2 + dy))
    }
  }

  def run(elves: Array[Elf], map: Set[P], currDir: Int): (Array[Elf], Set[P], Boolean, Int) = {
    val proposes = mutable.Map.empty[P, Int].withDefaultValue(0)
    val nextElves = elves.map { elf =>
      if (aroundAllEmpty(elf, map)) {
        elf
      } else {
        var nextPos: P = elf._1
        var moving = false
        for (i <- 0 until 4 if !moving) {
          val dir = ORDER((currDir + i) % 4)
          if (!elfInDirection(elf, map, dir)) {
            val (dx, dy) = DIRS(dir)
            nextPos = (elf._1._1 + dx, elf._1._2 + dy)
            proposes(nextPos) += 1
            moving = true
          }
        }
        (elf._1, moving, nextPos)
      }
    }

    var someoneMoved = false
    val nextMap = mutable.Set.empty[P]
    val finalElves = nextElves.map {
      case (pos, moving, nextPos) =>
        if (!moving) {
          nextMap.add(pos)
          (pos, false, pos)
        } else if (proposes(nextPos) > 1) {
          nextMap.add(pos)
          (pos, false, pos)
        } else {
          someoneMoved = true
          nextMap.add(nextPos)
          (nextPos, false, nextPos)
        }
    }
    (finalElves, nextMap.toSet, someoneMoved, (currDir + 1) % 4)
  }

  def minMax(map: Set[P]): (P, P) = {
    if (map.isEmpty) {
      ((0, 0), (0, 0))
    } else {
      val minX = map.map(_._1).min
      val maxX = map.map(_._1).max
      val minY = map.map(_._2).min
      val maxY = map.map(_._2).max
      ((minX, minY), (maxX, maxY))
    }
  }

  def parse(): (Array[Elf], Set[P]) = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    val map = mutable.Set.empty[P]
    val elves = mutable.ArrayBuffer.empty[Elf]
    for ((line, row) <- lines.zipWithIndex) {
      for ((char, col) <- line.zipWithIndex) {
        if (char == '#') {
          val p = (row, col)
          map.add(p)
          elves.append((p, false, p))
        }
      }
    }
    (elves.toArray, map.toSet)
  }

  def main(args: Array[String]): Unit = {
    var (elves, map) = parse()
    var currDir = 0
    for (_ <- 0 until 10) {
      val (nextElves, nextMap, _, nextCurrDir) = run(elves, map, currDir)
      elves = nextElves
      map = nextMap
      currDir = nextCurrDir
    }

    val (min, max) = minMax(map)
    val count = (min._1 to max._1).flatMap(x => (min._2 to max._2).map(y => (x, y))).count(!map.contains(_))
    println(count)
  }
}
