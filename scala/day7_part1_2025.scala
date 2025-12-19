import scala.io.Source
import java.io.File

object Main extends App {
  val lines = Source.fromFile(new File("input.txt")).getLines().filter(_.nonEmpty).toVector
  if (lines.isEmpty) { println("Empty grid"); System.exit(0) }

  val height = lines.length
  val width  = lines.head.length

  val (startX, startY) = {
    var sx = -1; var sy = -1; var found = false
    for (y <- 0 until height if !found) {
      for (x <- 0 until width if !found) {
        if (lines(y)(x) == 'S') { sx = x; sy = y; found = true }
      }
    }
    (sx, sy)
  }

  var active = Set(startX)
  var splits = 0

  for (y <- startY until height if active.nonEmpty) {
    var next = Set.empty[Int]
    for (x <- active if x >= 0 && x < width) {
      lines(y)(x) match {
        case '^' =>
          splits += 1
          if (x > 0) next += (x - 1)
          if (x + 1 < width) next += (x + 1)
        case _ => next += x
      }
    }
    active = next
  }

  println(s"Total times the beam is split: $splits")
}