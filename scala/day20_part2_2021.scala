
import scala.io.Source

val iterations = 50
val expandBy = 1

object Main extends App {
  val (algorithm, image) = readInput("input.txt")
  var currentImage = image
  for (i <- 0 until iterations) {
    currentImage = enhanceImage(algorithm, currentImage, i % 2 == 1 && algorithm(0) == '#')
  }
  println(countLitPixels(currentImage))
}

def readInput(filename: String): (Array[Char], Array[Array[Boolean]]) = {
  val source = Source.fromFile(filename)
  val lines = source.getLines().toArray
  source.close()

  val algorithm = lines(0).toCharArray
  val image = lines.drop(2).map(line => line.trim.map(_ == '#').toArray)
  (algorithm, image)
}

def enhanceImage(algorithm: Array[Char], image: Array[Array[Boolean]], useInfiniteLit: Boolean): Array[Array[Boolean]] = {
  val newImage = Array.ofDim[Boolean](image.length + (expandBy * 2), image(0).length + (expandBy * 2))

  for (y <- -expandBy until image.length + expandBy) {
    for (x <- -expandBy until image(0).length + expandBy) {
      var index = 0
      for (dy <- -1 to 1) {
        for (dx <- -1 to 1) {
          index <<= 1
          val ny = y + dy
          val nx = x + dx
          if (ny >= 0 && ny < image.length && nx >= 0 && nx < image(0).length) {
            if (image(ny)(nx)) {
              index |= 1
            }
          } else if (useInfiniteLit) {
            index |= 1
          }
        }
      }
      newImage(y + expandBy)(x + expandBy) = algorithm(index) == '#'
    }
  }
  newImage
}

def countLitPixels(image: Array[Array[Boolean]]): Int = {
  image.map(_.count(identity)).sum
}
