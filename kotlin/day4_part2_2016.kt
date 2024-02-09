import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    for (line in lines) {
        if (isRealRoom(line)) {
            val decryptedName = decryptName(line)
            if (decryptedName.contains("northpole object")) {
                println(getSectorID(line))
                break
            }
        }
    }
}

fun isRealRoom(room: String): Boolean {
    val parts = room.split("[")
    val checksum = parts[1].substring(0, parts[1].length - 1)
    val encryptedName = parts[0].split("-").dropLast(1)

    val letterCounts = mutableMapOf<Char, Int>()
    for (part in encryptedName) {
        for (letter in part) {
            letterCounts[letter] = letterCounts.getOrDefault(letter, 0) + 1
        }
    }

    val counts = letterCounts.entries.sortedWith(compareBy({ -it.value }, { it.key }))

    for (i in checksum.indices) {
        if (checksum[i] != counts[i].key) {
            return false
        }
    }

    return true
}

fun getSectorID(room: String): Int {
    val parts = room.split("-")
    val sectorIDPart = parts.last()
    val sectorID = sectorIDPart.split("[")[0].toInt()
    return sectorID
}

fun decryptName(room: String): String {
    val parts = room.split("-")
    val sectorIDPart = parts.last()
    val sectorID = sectorIDPart.split("[")[0].toInt()
    val decryptedName = StringBuilder()

    for (part in parts.dropLast(1)) {
        for (letter in part) {
            if (letter == '-') {
                decryptedName.append(' ')
            } else {
                val shiftedLetter = 'a' + ((letter - 'a' + sectorID) % 26)
                decryptedName.append(shiftedLetter)
            }
        }
        decryptedName.append(' ')
    }

    return decryptedName.toString().trim()
}