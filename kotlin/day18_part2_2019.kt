
import java.io.File
import java.util.*

fun main() {
    val originalMap = File("input.txt").readLines().map { it.toCharArray() }

    var found = false
    for (y in 1 until originalMap.size - 1) {
        for (x in 1 until originalMap[0].size - 1) {
            if (originalMap[y][x] == '@') {
                if (originalMap[y - 1][x] == '.' && originalMap[y + 1][x] == '.' &&
                    originalMap[y][x - 1] == '.' && originalMap[y][x + 1] == '.'
                ) {
                    originalMap[y - 1][x - 1] = '@'; originalMap[y - 1][x] = '#'; originalMap[y - 1][x + 1] = '@'
                    originalMap[y][x - 1] = '#'; originalMap[y][x] = '#'; originalMap[y][x + 1] = '#'
                    originalMap[y + 1][x - 1] = '@'; originalMap[y + 1][x] = '#'; originalMap[y + 1][x + 1] = '@'
                    found = true
                    break
                }
            }
        }
        if (found) break
    }

    if (!found) {
        println("Error: Could not find the '@' symbol surrounded by open spaces.")
        return
    }

    val robotPositions = mutableListOf<Pair<Int, Int>>()
    val keys = mutableMapOf<Char, Pair<Int, Int>>()
    val allKeys = mutableSetOf<Char>()
    val doors = mutableMapOf<Char, Pair<Int, Int>>()

    for ((y, row) in originalMap.withIndex()) {
        for ((x, cell) in row.withIndex()) {
            when {
                cell == '@' -> robotPositions.add(x to y)
                cell.isLowerCase() -> {
                    keys[cell] = x to y; allKeys.add(cell)
                }
                cell.isUpperCase() -> doors[cell] = x to y
            }
        }
    }

    fun bfs(startPos: Pair<Int, Int>): Map<Char, Pair<Int, Set<Char>>> {
        val queue: Queue<Triple<Int, Int, Pair<Int, Set<Char>>>> = LinkedList()
        queue.add(Triple(startPos.first, startPos.second, 0 to emptySet()))
        val visited = mutableSetOf<Pair<Int, Int>>()
        val results = mutableMapOf<Char, Pair<Int, Set<Char>>>()

        while (queue.isNotEmpty()) {
            val (x, y, distAndReq) = queue.poll()
            val (dist, requiredKeys) = distAndReq
            if (x to y in visited) continue
            visited.add(x to y)

            val cell = originalMap[y][x]
            if (cell.isLowerCase() && cell !in requiredKeys) {
                results[cell] = dist to requiredKeys
                val newRequiredKeys = requiredKeys + cell
                
                for ((dx, dy) in listOf(-1 to 0, 1 to 0, 0 to -1, 0 to 1)) {
                    val nx = x + dx
                    val ny = y + dy
                    if (ny in originalMap.indices && nx in originalMap[0].indices) {
                        val ncell = originalMap[ny][nx]
                        if (ncell != '#') {
                            if (ncell.isUpperCase()) {
                                queue.add(Triple(nx, ny, (dist + 1) to (newRequiredKeys + ncell.toLowerCase())))
                            }
                             else {
                                queue.add(Triple(nx,ny, (dist + 1) to newRequiredKeys))
                             }
                        }
                    }

                }
                continue
            }
            

            for ((dx, dy) in listOf(-1 to 0, 1 to 0, 0 to -1, 0 to 1)) {
                val nx = x + dx
                val ny = y + dy
                if (ny in originalMap.indices && nx in originalMap[0].indices) {
                    val ncell = originalMap[ny][nx]
                    if (ncell != '#') {
                        if (ncell.isUpperCase()) {
                            queue.add(Triple(nx, ny, (dist + 1) to (requiredKeys + ncell.toLowerCase())))
                        } else {
                            queue.add(Triple(nx, ny, dist + 1 to requiredKeys))
                        }
                    }
                }
            }
        }
        return results
    }

    val keyPositions = keys.toMutableMap()
    robotPositions.forEachIndexed { index, pos -> keyPositions['@' + index] = pos }

    fun keyBfs(startKey: Char): Map<Char, Pair<Int, Set<Char>>> {
        val startPos = keyPositions[startKey]!!
        val queue: Queue<Triple<Int, Int, Pair<Int, Set<Char>>>> = LinkedList()
        queue.add(Triple(startPos.first, startPos.second, 0 to emptySet()))
        val visited = mutableSetOf<Pair<Int, Int>>()
        val results = mutableMapOf<Char, Pair<Int, Set<Char>>>()

        while (queue.isNotEmpty()) {
            val (x, y, distAndReq) = queue.poll()
            val (dist, requiredKeys) = distAndReq

            if (x to y in visited) continue
            visited.add(x to y)

            val cell = originalMap[y][x]
            if (cell.isLowerCase() && cell != startKey && cell !in requiredKeys) {
                results[cell] = dist to requiredKeys
                val newRequiredKeys = requiredKeys + cell
                for ((dx, dy) in listOf(-1 to 0, 1 to 0, 0 to -1, 0 to 1)) {
                    val nx = x + dx
                    val ny = y + dy
                    if (ny in originalMap.indices && nx in originalMap[0].indices && originalMap[ny][nx] != '#') {
                         val ncell = originalMap[ny][nx]
                         if (ncell.isUpperCase()){
                            queue.add(Triple(nx, ny, (dist + 1) to (newRequiredKeys + ncell.toLowerCase())))
                         } else {
                            queue.add(Triple(nx,ny, (dist + 1) to newRequiredKeys))
                         }
                    }
                }
                 continue;

            }

            for ((dx, dy) in listOf(-1 to 0, 1 to 0, 0 to -1, 0 to 1)) {
                val nx = x + dx
                val ny = y + dy
                if (ny in originalMap.indices && nx in originalMap[0].indices && originalMap[ny][nx] != '#') {
                    val ncell = originalMap[ny][nx]
                    if (ncell.isUpperCase()) {
                        queue.add(Triple(nx, ny, (dist + 1) to (requiredKeys + ncell.toLowerCase())))
                    } else {
                        queue.add(Triple(nx, ny, dist + 1 to requiredKeys))
                    }
                }
            }
        }
        return results
    }

    val keyGraph = mutableMapOf<Char, Map<Char, Pair<Int, Set<Char>>>>()
    val allNodes = keys.keys + robotPositions.mapIndexed { index, _ -> '@' + index }
    allNodes.forEach { key -> keyGraph[key] = keyBfs(key) }

    fun bitmask(keysSet: Set<Char>): Int {
        var mask = 0
        for (k in keysSet) {
            mask = mask or (1 shl (k - 'a'))
        }
        return mask
    }

    fun dijkstra(): Int? {
        val totalKeys = allKeys.size
        val initialPositions = robotPositions.indices.map { '@' + it }
        val initialState = initialPositions to emptySet<Char>()
        val heap = PriorityQueue<Triple<Int, List<Char>, Set<Char>>>(compareBy { it.first })
        heap.add(Triple(0, initialPositions, emptySet()))
        val visited = mutableMapOf<Pair<List<Char>, Int>, Int>()

        while (heap.isNotEmpty()) {
            val (cost, positions, collectedKeys) = heap.poll()
            val state = positions to bitmask(collectedKeys)

            if (state in visited && visited[state]!! <= cost) continue
            visited[state] = cost

            if (collectedKeys.size == totalKeys) return cost

            for (i in positions.indices) {
                val pos = positions[i]
                keyGraph[pos]?.forEach { (key, distAndReq) ->
                    val (dist, requiredKeys) = distAndReq
                    if (key !in collectedKeys && requiredKeys.all { it in collectedKeys }) {
                        val newPositions = positions.toMutableList().also { it[i] = key }
                        val newCollectedKeys = collectedKeys + key
                        val newState = newPositions to bitmask(newCollectedKeys)
                        if (newState !in visited || visited[newState]!! > cost + dist) {
                            heap.add(Triple(cost + dist, newPositions, newCollectedKeys))
                        }
                    }
                }
            }
        }
        return null
    }

    println(dijkstra())
}
