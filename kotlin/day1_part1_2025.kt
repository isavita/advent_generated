import java.io.File

fun main() {
    val tokens = File("input.txt").readText().trim().split(Regex("\\s+"))
    var pos = 50
    var cnt = 0
    for (t in tokens) {
        if (t.isEmpty()) continue
        val dir = t[0]
        val amt = t.substring(1).toInt()
        pos = (pos + if (dir == 'R') amt else -amt) % 100
        if (pos < 0) pos += 100
        if (pos == 0) cnt++
    }
    println(cnt)
}