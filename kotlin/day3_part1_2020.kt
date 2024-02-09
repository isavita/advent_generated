import java.io.File

fun main(args: Array<String>) {
    val forest = mutableListOf<String>()
    File("input.txt").forEachLine { line ->
        forest.add(line)
    }

    val trees = countTrees(forest, 3, 1)
    println(trees)
}

fun countTrees(forest: List<String>, right: Int, down: Int): Int {
    var trees = 0
    var x = 0
    val width = forest[0].length

    for (y in forest.indices step down) {
        if (forest[y][x % width] == '#') {
            trees++
        }
        x += right
    }

    return trees
}