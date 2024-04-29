import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val (algorithm, image) = readInput("input.txt")
    val enhancedImage = enhanceImage(image, algorithm, 2)
    println(countLitPixels(enhancedImage))
  }

  def readInput(filename: String): (String, Array[Array[Char]]) = {
    val lines = Source.fromFile(filename).getLines().toArray
    val algorithm = lines.head.replace("\n", "")
    val image = lines.tail.tail.map(_.toArray)
    (algorithm, image)
  }

  def enhanceImage(image: Array[Array[Char]], algorithm: String, times: Int): Array[Array[Char]] = {
    var enhancedImage = image
    for (i <- 0 until times) {
      enhancedImage = applyAlgorithm(enhancedImage, algorithm, i % 2 == 1 && algorithm.head == '#')
    }
    enhancedImage
  }

  def applyAlgorithm(image: Array[Array[Char]], algorithm: String, flip: Boolean): Array[Array[Char]] = {
    val enhancedImage = Array.fill(image.length + 2, image.head.length + 2)('.')
    for (i <- 0 until enhancedImage.length) {
      for (j <- 0 until enhancedImage.head.length) {
        val index = calculateIndex(i - 1, j - 1, image, flip)
        enhancedImage(i)(j) = algorithm(index)
      }
    }
    enhancedImage
  }

  def calculateIndex(i: Int, j: Int, image: Array[Array[Char]], flip: Boolean): Int = {
    var index = 0
    for (di <- -1 to 1) {
      for (dj <- -1 to 1) {
        index <<= 1
        if (i + di >= 0 && i + di < image.length && j + dj >= 0 && j + dj < image.head.length) {
          if (image(i + di)(j + dj) == '#') {
            index |= 1
          }
        } else if (flip) {
          index |= 1
        }
      }
    }
    index
  }

  def countLitPixels(image: Array[Array[Char]]): Int = {
    image.flatten.count(_ == '#')
  }
}