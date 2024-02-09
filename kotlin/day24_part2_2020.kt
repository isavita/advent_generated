import java.io.File

fun main() {
    val input = File("input.txt").readLines()

    val blackTiles = mutableSetOf<Pair<Int, Int>>()

    for (line in input) {
        var x = 0
        var y = 0
        var i = 0
        while (i < line.length) {
            when {
                line[i] == 'e' -> { x += 1; i += 1 }
                line[i] == 'w' -> { x -= 1; i += 1 }
                line[i] == 'n' && line[i + 1] == 'e' -> { x += 1; y -= 1; i += 2 }
                line[i] == 'n' && line[i + 1] == 'w' -> { y -= 1; i += 2 }
                line[i] == 's' && line[i + 1] == 'e' -> { y += 1; i += 2 }
                line[i] == 's' && line[i + 1] == 'w' -> { x -= 1; y += 1; i += 2 }
            }
        }
        val tile = Pair(x, y)
        if (tile in blackTiles) {
            blackTiles.remove(tile)
        } else {
            blackTiles.add(tile)
        }
    }

    repeat(100) {
        val newBlackTiles = mutableSetOf<Pair<Int, Int>>()
        val whiteTiles = mutableSetOf<Pair<Int, Int>>()

        for (tile in blackTiles) {
            val (x, y) = tile
            var blackAdjacent = 0

            if (Pair(x + 1, y) in blackTiles) blackAdjacent++
            if (Pair(x - 1, y) in blackTiles) blackAdjacent++
            if (Pair(x, y - 1) in blackTiles) blackAdjacent++
            if (Pair(x, y + 1) in blackTiles) blackAdjacent++
            if (Pair(x + 1, y - 1) in blackTiles) blackAdjacent++
            if (Pair(x - 1, y + 1) in blackTiles) blackAdjacent++

            if (blackAdjacent == 1 || blackAdjacent == 2) {
                newBlackTiles.add(tile)
            }

            val neighbors = setOf(
                Pair(x + 1, y),
                Pair(x - 1, y),
                Pair(x, y - 1),
                Pair(x, y + 1),
                Pair(x + 1, y - 1),
                Pair(x - 1, y + 1)
            )

            for (neighbor in neighbors) {
                if (neighbor !in blackTiles) {
                    var blackAdjacent = 0

                    if (Pair(neighbor.first + 1, neighbor.second) in blackTiles) blackAdjacent++
                    if (Pair(neighbor.first - 1, neighbor.second) in blackTiles) blackAdjacent++
                    if (Pair(neighbor.first, neighbor.second - 1) in blackTiles) blackAdjacent++
                    if (Pair(neighbor.first, neighbor.second + 1) in blackTiles) blackAdjacent++
                    if (Pair(neighbor.first + 1, neighbor.second - 1) in blackTiles) blackAdjacent++
                    if (Pair(neighbor.first - 1, neighbor.second + 1) in blackTiles) blackAdjacent++

                    if (blackAdjacent == 2) {
                        newBlackTiles.add(neighbor)
                    } else {
                        whiteTiles.add(neighbor)
                    }
                }
            }
        }

        blackTiles.clear()
        blackTiles.addAll(newBlackTiles)
    }

    println(blackTiles.size)
}