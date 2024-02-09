import java.io.File

fun reverseSection(arr: IntArray, start: Int, length: Int) {
    val n = arr.size
    for (i in start until start + length / 2) {
        val temp = arr[i % n]
        arr[i % n] = arr[(start + length - 1 - (i - start)) % n]
        arr[(start + length - 1 - (i - start)) % n] = temp
    }
}

fun knotHash(input: String): String {
    val lengths = input.map { it.toInt() } + listOf(17, 31, 73, 47, 23)
    val list = IntArray(256) { it }

    var position = 0
    var skip = 0
    repeat(64) {
        lengths.forEach { length ->
            reverseSection(list, position, length)
            position = (position + length + skip) % 256
            skip++
        }
    }

    val denseHash = List(16) { i ->
        list.slice(i * 16 until (i + 1) * 16).reduce { acc, value -> acc xor value }
    }

    return denseHash.joinToString("") { it.toString(16).padStart(2, '0') }
}

fun hexToBinary(hexStr: String): String {
    return hexStr.map {
        Integer.parseInt(it.toString(), 16).toString(2).padStart(4, '0')
    }.joinToString("")
}

fun dfs(x: Int, y: Int, grid: Array<IntArray>) {
    if (x < 0 || x >= 128 || y < 0 || y >= 128 || grid[x][y] != 1) {
        return
    }
    grid[x][y] = 0
    dfs(x - 1, y, grid)
    dfs(x + 1, y, grid)
    dfs(x, y - 1, grid)
    dfs(x, y + 1, grid)
}

fun main(args: Array<String>) {
    val keyString = File("input.txt").readText().trim()
    val grid = Array(128) { IntArray(128) }
    var totalUsed = 0
    var regions = 0

    for (i in grid.indices) {
        val rowKey = "$keyString-$i"
        val hash = knotHash(rowKey)
        val binaryRow = hexToBinary(hash)

        binaryRow.forEachIndexed { j, bit ->
            if (bit == '1') {
                grid[i][j] = 1
                totalUsed++
            }
        }
    }

    for (i in 0 until 128) {
        for (j in 0 until 128) {
            if (grid[i][j] == 1) {
                regions++
                dfs(i, j, grid)
            }
        }
    }

    println(regions)
}