
import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().mkString("\n")
    val lines = input.split("\n")

    var ground = Array.ofDim[Char](1, 1)
    ground(0)(0) = '+'

    var maxX = 0
    var minX = 0
    var maxY = 0
    var minY = 20
    var xOffset = 500
    var yOffset = 0

    for (line <- lines) {
      val split = line.split("[=, .]+")
      if (split(0) == "x") {
        val x = split(1).toInt - xOffset
        val y1 = split(3).toInt - yOffset
        val y2 = split(4).toInt - yOffset

        while (x >= maxX) {
          maxX += 1
          ground = ground.map(row => row :+ '.')
        }
        while (x <= minX) {
          minX -= 1
          ground = ground.map(row => '.' +: row)
        }
        while (y2 > maxY) {
          maxY += 1
          ground :+= Array.fill(ground(0).length)('.')
        }
        if (y1 < minY) {
          minY = y1
        }
        for (i <- y1 to y2) {
          ground(i)(x - minX) = '#'
        }
      } else {
        val y = split(1).toInt - yOffset
        val x1 = split(3).toInt - xOffset
        val x2 = split(4).toInt - xOffset

        while (y > maxY) {
          maxY += 1
          ground :+= Array.fill(ground(0).length)('.')
        }
        while (x2 >= maxX) {
          maxX += 1
          ground = ground.map(row => row :+ '.')
        }
        while (x1 <= minX) {
          minX -= 1
          ground = ground.map(row => '.' +: row)
        }
        for (i <- x1 to x2) {
          ground(y)(i - minX) = '#'
        }
        if (y < minY) {
          minY = y
        }
      }
    }

    var waterCount = 0
    var flowCount = 0
    val roundLimit = 200000

    while (ground(1)(-minX) != '|' && waterCount < roundLimit) {
      var canMove = true
      var x = -minX
      var y = 1
      var tryLeft = 0
      while (canMove) {
        if (y + 1 > maxY || ground(y + 1)(x) == '|') {
          ground(y)(x) = '|'
          canMove = false
          if (y >= minY) {
            flowCount += 1
          }
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
            for (i <- x + 1 until ground(y).length if ground(y)(i) == '~') {
              ground(y)(i) = '|'
              waterCount -= 1
              flowCount += 1
            }
            for (i <- x - 1 to 0 by -1 if ground(y)(i) == '~') {
              ground(y)(i) = '|'
              waterCount -= 1
              flowCount += 1
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
    println(flowCount + waterCount)
  }
}
