import java.io.File

fun main() {
    // Read input from "input.txt"
    val line = File("input.txt").readText().trim()

    // Build disk list from input
    val disk = mutableListOf<String>()
    var fileId = 0
    var isFile = true
    for (char in line) {
        val length = char.toString().toInt()
        if (isFile) {
            disk.addAll(List(length) { fileId.toString() })
            fileId++
        } else {
            disk.addAll(List(length) { "." })
        }
        isFile = !isFile
    }

    // Move files left by swapping free spaces and files
    while (true) {
        val lfree = disk.indexOfFirst { it == "." }
        if (lfree == -1) break

        // Find the last file (non ".") from the right, starting after lfree
        val rfile = (disk.size - 1 downTo lfree + 1).firstOrNull { disk[it] != "." } ?: -1
        if (rfile == -1) break

        // Swap: move file to free spot, mark original as free
        disk[lfree] = disk[rfile].also { disk[rfile] = "." }
    }

    // Calculate checksum: sum(i * fileId) for non-free positions
    var checksum = 0L
    disk.forEachIndexed { index, value ->
        if (value != ".") {
            checksum += index * value.toInt()
        }
    }
    println(checksum)
}
