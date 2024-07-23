
import scala.io.Source

object ImageDecoder {
  def main(args: Array[String]): Unit = {
    val imageData = Source.fromFile("input.txt").getLines().mkString.trim
    val (width, height) = (25, 6)
    val layerSize = width * height
    val finalImage = Array.fill(layerSize)('2')

    for (i <- imageData.indices by layerSize) {
      val layer = imageData.slice(i, math.min(i + layerSize, imageData.length))

      for (j <- layer.indices) {
        if (finalImage(j) == '2') finalImage(j) = layer(j)
      }
    }

    println("Decoded image:")
    for (i <- 0 until height) {
      for (j <- 0 until width) {
        val pixel = finalImage(i * width + j)
        print(if (pixel == '0') " " else "#")
      }
      println()
    }
  }
}
