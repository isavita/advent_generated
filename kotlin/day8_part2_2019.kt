
import java.io.File

fun main() {
    val imageData = File("input.txt").readText().trim()
    val width = 25
    val height = 6
    val layerSize = width * height
    val finalImage = CharArray(layerSize) { '2' }

    for (i in 0 until imageData.length step layerSize) {
        val layer = imageData.substring(i, minOf(i + layerSize, imageData.length))
        for (j in layer.indices) {
            if (finalImage[j] == '2') {
                finalImage[j] = layer[j]
            }
        }
    }

    println("Decoded image:")
    for (i in 0 until height) {
        for (j in 0 until width) {
            val pixel = finalImage[i * width + j]
            print(if (pixel == '0') " " else if (pixel == '1') "#" else "")
        }
        println()
    }
}
