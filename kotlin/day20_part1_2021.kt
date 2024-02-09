import java.io.File

fun main(args: Array<String>) {
    val (algorithm, image) = readInput("input.txt")
    var enhancedImage = enhanceImage(image, algorithm, 2)
    println(countLitPixels(enhancedImage))
}

fun readInput(filename: String): Pair<String, List<CharArray>> {
    val lines = File(filename).readLines()
    val algorithm = lines[0].replace("\n", "")
    val image = lines.subList(2, lines.size).map { it.toCharArray() }
    return Pair(algorithm, image)
}

fun enhanceImage(image: List<CharArray>, algorithm: String, times: Int): List<CharArray> {
    var enhancedImage = image
    repeat(times) {
        enhancedImage = applyAlgorithm(enhancedImage, algorithm, it % 2 == 1 && algorithm[0] == '#')
    }
    return enhancedImage
}

fun applyAlgorithm(image: List<CharArray>, algorithm: String, flip: Boolean): List<CharArray> {
    val enhancedImage = Array(image.size + 2) { CharArray(image[0].size + 2) }
    for (i in enhancedImage.indices) {
        for (j in enhancedImage[i].indices) {
            val index = calculateIndex(i - 1, j - 1, image, flip)
            enhancedImage[i][j] = algorithm[index]
        }
    }
    return enhancedImage.toList()
}

fun calculateIndex(i: Int, j: Int, image: List<CharArray>, flip: Boolean): Int {
    var index = 0
    for (di in -1..1) {
        for (dj in -1..1) {
            index = index shl 1
            if (i + di in image.indices && j + dj in image[0].indices) {
                if (image[i + di][j + dj] == '#') {
                    index = index or 1
                }
            } else if (flip) {
                index = index or 1
            }
        }
    }
    return index
}

fun countLitPixels(image: List<CharArray>): Int {
    var count = 0
    for (row in image) {
        for (pixel in row) {
            if (pixel == '#') {
                count++
            }
        }
    }
    return count
}