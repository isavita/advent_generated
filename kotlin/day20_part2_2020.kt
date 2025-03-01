
import java.io.File
import kotlin.math.sqrt

data class Tile(val id: Int, val data: List<String>) {
    fun edges(): List<String> {
        val top = data.first()
        val bottom = data.last()
        val left = data.map { it.first() }.joinToString("")
        val right = data.map { it.last() }.joinToString("")
        return listOf(top, bottom, left, right)
    }

    fun allEdges(): List<String> {
        val edges = edges()
        return edges + edges.map { it.reversed() }
    }

    fun rotate(): Tile {
        val newData = List(data.size) { row ->
            (0 until data.size).map { col ->
                data[data.size - 1 - col][row]
            }.joinToString("")
        }
        return Tile(id, newData)
    }

    fun flip(): Tile {
        return Tile(id, data.reversed())
    }

    fun trimBorder(): List<String> {
        return data.subList(1, data.size - 1).map { it.substring(1, it.length - 1) }
    }
}

fun main() {
    val tiles = parseTiles("input.txt")
    val solution = solvePuzzle(tiles)
    val part1 = solution.keys.filter { isCorner(it, solution) }.map { it.id.toLong() }.reduce { acc, i -> acc * i }
    println("Part 1: $part1")

    val image = assembleImage(solution)
    val roughness = calculateWaterRoughness(image)
    println("Part 2: $roughness")
}

fun parseTiles(filename: String): List<Tile> {
    val lines = File(filename).readLines()
    val tiles = mutableListOf<Tile>()
    var i = 0
    while (i < lines.size) {
        val id = lines[i].substring(5, lines[i].length - 1).toInt()
        val data = lines.subList(i + 1, i + 11)
        tiles.add(Tile(id, data))
        i += 12
    }
    return tiles
}

fun solvePuzzle(tiles: List<Tile>): Map<Tile, Pair<Int, Int>> {
    val size = sqrt(tiles.size.toDouble()).toInt()
    val solution = mutableMapOf<Tile, Pair<Int, Int>>()
    val used = mutableSetOf<Int>()
    fun findSolution(x: Int, y: Int): Boolean {
        if (y == size) return true
        val nextX = if (x == size - 1) 0 else x + 1
        val nextY = if (x == size - 1) y + 1 else y

        for (tile in tiles) {
            if (tile.id !in used) {
                for (orientation in tile.orientations()) {
                    if (canPlace(orientation, x, y, solution)) {
                        solution[orientation] = x to y
                        used.add(tile.id)
                        if (findSolution(nextX, nextY)) return true
                        solution.remove(orientation)
                        used.remove(tile.id)
                    }
                }
            }
        }
        return false
    }

    findSolution(0, 0)
    return solution
}

fun Tile.orientations(): List<Tile> {
    val orientations = mutableListOf<Tile>()
    var current = this
    repeat(4) {
        orientations.add(current)
        orientations.add(current.flip())
        current = current.rotate()
    }
    return orientations
}

fun canPlace(tile: Tile, x: Int, y: Int, solution: Map<Tile, Pair<Int, Int>>): Boolean {
    val leftNeighbor = solution.entries.find { it.value == (x - 1) to y }
    if (leftNeighbor != null && leftNeighbor.key.edges()[3] != tile.edges()[2]) return false

    val topNeighbor = solution.entries.find { it.value == x to (y - 1) }
    if (topNeighbor != null && topNeighbor.key.edges()[1] != tile.edges()[0]) return false

    return true
}

fun isCorner(tile: Tile, solution: Map<Tile, Pair<Int, Int>>): Boolean {
    val (x, y) = solution[tile]!!
    val size = sqrt(solution.size.toDouble()).toInt()
    return (x == 0 || x == size - 1) && (y == 0 || y == size - 1)
}

fun assembleImage(solution: Map<Tile, Pair<Int, Int>>): List<String> {
    val tileSize = solution.keys.first().data.size - 2
    val gridSize = sqrt(solution.size.toDouble()).toInt()
    val image = mutableListOf<String>()

    for (gridRow in 0 until gridSize) {
        val rowTiles = solution.entries.filter { it.value.second == gridRow }.sortedBy { it.value.first }
        for (i in 0 until tileSize) {
            val rowData = rowTiles.joinToString("") { it.key.trimBorder()[i] }
            image.add(rowData)
        }
    }
    return image
}
fun calculateWaterRoughness(image: List<String>): Int {
    val monsterPattern = listOf(
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
    )
    val monsterWidth = monsterPattern[0].length
    val monsterHeight = monsterPattern.size
    val monsterHashCount = monsterPattern.sumOf { row -> row.count { it == '#' } }

    var totalHashCount = 0
    var monsterCount = 0

     fun countMonsters(img: List<String>): Int {
        var count = 0
        for (y in 0..img.size - monsterHeight) {
            for (x in 0..img[0].length - monsterWidth) {
                if (isMonsterAt(img, x, y, monsterPattern)) {
                    count++
                }
            }
        }
         return count;
    }


    val imageOrientations = imageOrientations(image)
    for (orientedImage in imageOrientations) {
        val currentMonsterCount = countMonsters(orientedImage)
         if (currentMonsterCount > 0){
                monsterCount = currentMonsterCount
                totalHashCount = orientedImage.sumOf { line -> line.count { it == '#' } }
                break;
         }
    }


    return totalHashCount - monsterCount * monsterHashCount
}

fun isMonsterAt(image: List<String>, x: Int, y: Int, monsterPattern: List<String>): Boolean {
    for (dy in 0 until monsterPattern.size) {
        for (dx in 0 until monsterPattern[0].length) {
            if (monsterPattern[dy][dx] == '#' && image[y + dy][x + dx] != '#') {
                return false
            }
        }
    }
    return true
}

fun imageOrientations(image: List<String>): List<List<String>> {
    val orientations = mutableListOf<List<String>>()
    var current = image
    repeat(4) {
        orientations.add(current)
        orientations.add(current.reversed()) // Flip vertically
          orientations.add(current.map { it.reversed() }) //flip horizontally.
        current = rotateImage(current)
    }
    return orientations.distinct()
}

fun rotateImage(image: List<String>): List<String> {
    val n = image.size
    val rotated = MutableList(n) { MutableList(n) { ' ' } }
    for (i in 0 until n) {
        for (j in 0 until n) {
            rotated[j][n - 1 - i] = image[i][j]
        }
    }
    return rotated.map { it.joinToString("") }
}

