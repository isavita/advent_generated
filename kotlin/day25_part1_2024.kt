
import java.io.File

fun main() {
    val raw = File("input.txt").readLines().filter { it.isNotBlank() }

    if (raw.size % 7 != 0) {
        println(0)
        return
    }

    val locks = mutableListOf<IntArray>()
    val keys = mutableListOf<IntArray>()

    for (i in 0 until raw.size step 7) {
        val block = raw.subList(i, i + 7)
        if (block.any { it.length < 5 }) continue

        if (block[0].all { it == '#' }) {
            locks.add(parseLock(block))
        } else {
            keys.add(parseKey(block))
        }
    }

    val count = locks.sumOf { lock ->
        keys.count { key -> fits(lock, key) }
    }
    println(count)
}

fun parseLock(block: List<String>): IntArray {
    val h = IntArray(5)
    for (c in 0 until 5) {
        var cnt = 0
        for (r in 1 until 7) {
            if (block[r][c] == '#') {
                cnt++
            } else {
                break
            }
        }
        h[c] = cnt
    }
    return h
}

fun parseKey(block: List<String>): IntArray {
    val h = IntArray(5)
    for (c in 0 until 5) {
        var cnt = 0
        for (r in 5 downTo 0) {
            if (block[r][c] == '#') {
                cnt++
            } else {
                break
            }
        }
        h[c] = cnt
    }
    return h
}

fun fits(lock: IntArray, key: IntArray): Boolean {
    for (i in 0 until 5) {
        if (lock[i] + key[i] > 5) {
            return false
        }
    }
    return true
}
