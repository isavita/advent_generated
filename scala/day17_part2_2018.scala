
import scala.io.Source

object WaterFlow {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    var ground = Array.ofDim[Char](1, 1)
    ground(0)(0) = '+'
    var (maxX, minX, maxY, minY) = (0, 0, 0, 20)
    val (xOffset, yOffset) = (500, 0)

    for (line <- lines) {
      val split = line.split("[=, .]+")
      if (split(0) == "x") {
        val x = split(1).toInt - xOffset
        val (y1, y2) = (split(3).toInt - yOffset, split(4).toInt - yOffset)

        while (x >= maxX) {
          maxX += 1
          for (j <- ground.indices) ground(j) = ground(j) :+ '.'
        }
        while (x <= minX) {
          minX -= 1
          for (j <- ground.indices) ground(j) = '.' +: ground(j)
        }
        while (y2 > maxY) {
          maxY += 1
          ground = ground :+ Array.fill(ground(0).length)('.')
        }
        if (y1 < minY) minY = y1
        for (i <- y1 to y2) ground(i)(x - minX) = '#'
      } else {
        val y = split(1).toInt - yOffset
        val (x1, x2) = (split(3).toInt - xOffset, split(4).toInt - xOffset)

        while (y > maxY) {
          maxY += 1
          ground = ground :+ Array.fill(ground(0).length)('.')
        }
        while (x2 >= maxX) {
          maxX += 1
          for (j <- ground.indices) ground(j) = ground(j) :+ '.'
        }
        while (x1 <= minX) {
          minX -= 1
          for (j <- ground.indices) ground(j) = '.' +: ground(j)
        }
        for (i <- x1 to x2) ground(y)(i - minX) = '#'
        if (y < minY) minY = y
      }
    }

    var (waterCount, flowCount, roundLimit) = (0, 0, 200000)

    while (ground(1)(-minX) != '|' && waterCount < roundLimit) {
      var (canMove, x, y, tryLeft) = (true, -minX, 1, 0)
      while (canMove) {
        if (y + 1 > maxY || ground(y + 1)(x) == '|') {
          ground(y)(x) = '|'
          canMove = false
          if (y >= minY) flowCount += 1
        } else if (ground(y + 1)(x) == '.') {
          y += 1
          tryLeft = 0
        } else if (ground(y + 1)(x) == '#' || ground(y + 1)(x) == '~') {
          if ((tryLeft == 1 && ground(y)(x - 1) == '|') ||
            (tryLeft == 2 && ground(y)(x + 1) == '|') ||
            (ground(y)(x + 1) == '|' && ground(y)(x - 1) != '.') ||
            (ground(y)(x + 1) != '.' && ground(y)(x - 1) == '|')) {
            ground(y)(x) = '|'
            flowCount += 1
            canMove = false
            var i = x + 1
            while (i < ground(y).length && ground(y)(i) == '~') {
              ground(y)(i) = '|'
              waterCount -= 1
              flowCount += 1
              i += 1
            }
            i = x - 1
            while (i >= 0 && ground(y)(i) == '~') {
              ground(y)(i) = '|'
              waterCount -= 1
              flowCount += 1
              i -= 1
            }
          } else if ((tryLeft == 0 && ground(y)(x - 1) == '.') ||
            (tryLeft == 1 && ground(y)(x - 1) == '.')) {
            x -= 1
            tryLeft = 1
          } else if ((tryLeft == 0 && ground(y)(x + 1) == '.') ||
            (tryLeft == 2 && ground(y)(x + 1) == '.')) {
            x += 1
            tryLeft = 2
          } else {
            canMove = false
            ground(y)(x) = '~'
            waterCount += 1
          }
        }
      }
    }
    println(waterCount)
  }
}
