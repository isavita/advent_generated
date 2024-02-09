import java.io.File
import java.security.MessageDigest

fun main() {
    val passcode = File("input.txt").readText().trim()

    fun isOpen(char: Char): Boolean = char in 'b'..'f'

    fun getOpenDoors(path: String): List<Char> {
        val md5 = MessageDigest.getInstance("MD5")
        val digest = md5.digest((passcode + path).toByteArray()).joinToString("") { "%02x".format(it) }
        return listOf('U', 'D', 'L', 'R').filterIndexed { index, _ -> isOpen(digest[index]) }
    }

    data class State(val x: Int, val y: Int, val path: String)

    val queue = mutableListOf(State(0, 0, ""))
    val directions = listOf('U', 'D', 'L', 'R')
    var result = ""

    while (queue.isNotEmpty()) {
        val (x, y, path) = queue.removeAt(0)

        if (x == 3 && y == 3) {
            result = path
            break
        }

        val openDoors = getOpenDoors(path)
        for (dir in openDoors) {
            val (dx, dy) = when (dir) {
                'U' -> Pair(0, -1)
                'D' -> Pair(0, 1)
                'L' -> Pair(-1, 0)
                'R' -> Pair(1, 0)
                else -> Pair(0, 0)
            }

            val newX = x + dx
            val newY = y + dy

            if (newX in 0..3 && newY in 0..3) {
                queue.add(State(newX, newY, path + dir))
            }
        }
    }

    println(result)
}