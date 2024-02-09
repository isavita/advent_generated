import java.io.File

fun main(args: Array<String>) {
    val inputFileName = "input.txt"
    val input = File(inputFileName).readLines()
    val algorithm = input[0]
    val image = input.subList(2, input.size).map { it.map { c -> c == '#' } }

    var enhancedImage = image
    for (i in 0 until 50) {
        enhancedImage = enhanceImage(algorithm, enhancedImage, i % 2 == 1 && algorithm[0] == '#')
    }

    println(countLitPixels(enhancedImage))
}

fun enhanceImage(algorithm: String, image: List<List<Boolean>>, useInfiniteLit: Boolean): List<List<Boolean>> {
    val expandBy = 1
    val newImage = MutableList(image.size + (expandBy * 2)) { MutableList(image[0].size + (expandBy * 2)) { false } }

    for (y in -expandBy until image.size + expandBy) {
        for (x in -expandBy until image[0].size + expandBy) {
            var index = 0
            for (dy in -1..1) {
                for (dx in -1..1) {
                    index = index shl 1
                    val ny = y + dy
                    val nx = x + dx
                    if (ny >= 0 && ny < image.size && nx >= 0 && nx < image[0].size) {
                        if (image[ny][nx]) {
                            index = index or 1
                        }
                    } else if (useInfiniteLit) {
                        index = index or 1
                    }
                }
            }
            newImage[y + expandBy][x + expandBy] = algorithm[index] == '#'
        }
    }
    return newImage
}

fun countLitPixels(image: List<List<Boolean>>): Int {
    return image.sumBy { row -> row.count { it } }
}