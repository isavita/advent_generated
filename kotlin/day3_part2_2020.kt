import java.io.File

fun main() {
    val input = File("input.txt").readLines()

    val slopes = listOf(
        Slope(1, 1),
        Slope(3, 1),
        Slope(5, 1),
        Slope(7, 1),
        Slope(1, 2)
    )

    val results = slopes.map { slope ->
        val trees = traverse(input, slope)
        println("Slope ${slope.right},${slope.down}: $trees trees")
        trees
    }

    val product = results.fold(1L) { acc, i -> acc * i }
    println("Product of trees: $product")
}

data class Slope(val right: Int, val down: Int)

fun traverse(input: List<String>, slope: Slope): Int {
    var x = 0
    var y = 0
    var trees = 0

    while (y < input.size) {
        if (input[y][x % input[y].length] == '#') {
            trees++
        }
        x += slope.right
        y += slope.down
    }

    return trees
}