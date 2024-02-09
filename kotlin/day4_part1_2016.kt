import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val sumOfSectorIDs = file.readLines()
        .filter { isRealRoom(it) }
        .sumBy { getSectorID(it) }

    println(sumOfSectorIDs)
}

fun isRealRoom(room: String): Boolean {
    val parts = room.split("[")
    val checksum = parts[1].dropLast(1)
    val encryptedName = parts[0].split("-").dropLast(1)

    val letterCounts = encryptedName.joinToString("").groupingBy { it }.eachCount()

    val counts = letterCounts.entries.map { Pair(it.key, it.value) }
        .sortedWith(compareBy({ -it.second }, { it.first }))

    for (i in checksum.indices) {
        if (checksum[i] != counts[i].first) {
            return false
        }
    }

    return true
}

fun getSectorID(room: String): Int {
    val parts = room.split("-")
    val sectorIDPart = parts.last()
    return sectorIDPart.split("[")[0].toInt()
}