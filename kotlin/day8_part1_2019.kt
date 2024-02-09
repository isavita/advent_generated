import java.io.File

fun main(args: Array<String>) {
    val data = File("input.txt").readText().trim()

    val width = 25
    val height = 6
    val layerSize = width * height

    var minZeros = layerSize + 1
    var result = 0

    for (i in data.indices step layerSize) {
        val layer = data.substring(i, minOf(i + layerSize, data.length))
        var zeroCount = 0
        var oneCount = 0
        var twoCount = 0

        for (pixel in layer) {
            when (pixel) {
                '0' -> zeroCount++
                '1' -> oneCount++
                '2' -> twoCount++
            }
        }

        if (zeroCount < minZeros) {
            minZeros = zeroCount
            result = oneCount * twoCount
        }
    }

    println(result)
}