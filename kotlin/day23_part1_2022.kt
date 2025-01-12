
import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val elves = mutableSetOf<Pair<Int, Int>>()
    for (y in input.indices) {
        for (x in input[y].indices) {
            if (input[y][x] == '#') {
                elves.add(Pair(x, y))
            }
        }
    }

    val directions = listOf(
        listOf(Pair(0, -1), Pair(1, -1), Pair(-1, -1)), // N, NE, NW
        listOf(Pair(0, 1), Pair(1, 1), Pair(-1, 1)),   // S, SE, SW
        listOf(Pair(-1, 0), Pair(-1, -1), Pair(-1, 1)), // W, NW, SW
        listOf(Pair(1, 0), Pair(1, -1), Pair(1, 1))    // E, NE, SE
    ).toMutableList()

    for (round in 1..10) {
        val proposedMoves = mutableMapOf<Pair<Int, Int>, Pair<Int, Int>>()
        val moveCounts = mutableMapOf<Pair<Int, Int>, Int>()

        for (elf in elves) {
            var hasNeighbor = false
            for (dy in -1..1) {
                for (dx in -1..1) {
                    if (dx == 0 && dy == 0) continue
                    if (elves.contains(Pair(elf.first + dx, elf.second + dy))) {
                        hasNeighbor = true
                        break
                    }
                }
                if (hasNeighbor) break
            }
            if (!hasNeighbor) continue

            var proposedMove: Pair<Int, Int>? = null
            for (dir in directions) {
                var canMove = true
                for (offset in dir) {
                    if (elves.contains(Pair(elf.first + offset.first, elf.second + offset.second))) {
                        canMove = false
                        break
                    }
                }
                if (canMove) {
                    proposedMove = Pair(elf.first + dir[0].first, elf.second + dir[0].second)
                    break
                }
            }
            if (proposedMove != null) {
                proposedMoves[elf] = proposedMove
                moveCounts[proposedMove] = moveCounts.getOrDefault(proposedMove, 0) + 1
            }
        }

        val newElves = mutableSetOf<Pair<Int, Int>>()
        for (elf in elves) {
            if (proposedMoves.containsKey(elf)) {
                val proposedMove = proposedMoves[elf]!!
                if (moveCounts[proposedMove] == 1) {
                    newElves.add(proposedMove)
                } else {
                    newElves.add(elf)
                }
            } else {
                newElves.add(elf)
            }
        }
        elves.clear()
        elves.addAll(newElves)
        directions.add(directions.removeAt(0))
    }

    val minX = elves.minOf { it.first }
    val maxX = elves.maxOf { it.first }
    val minY = elves.minOf { it.second }
    val maxY = elves.maxOf { it.second }

    val emptyTiles = (maxX - minX + 1) * (maxY - minY + 1) - elves.size
    println(emptyTiles)
}
