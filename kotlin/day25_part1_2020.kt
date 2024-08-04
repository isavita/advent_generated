fun main() {
    val publicKeyCard = readPublicKey("input.txt", 0)
    val publicKeyDoor = readPublicKey("input.txt", 1)

    val loopSizeCard = findLoopSize(publicKeyCard, 7)
    val loopSizeDoor = findLoopSize(publicKeyDoor, 7)

    val encryptionKey = calculateEncryptionKey(publicKeyDoor, loopSizeCard)
    println(encryptionKey)
}

fun readPublicKey(filename: String, index: Int): Long {
    val lines = java.io.File(filename).readLines()
    return lines[index].toLong()
}

fun findLoopSize(publicKey: Long, subjectNumber: Long): Int {
    var value = 1L
    var loopSize = 0
    while (true) {
        value = (value * subjectNumber) % 20201227
        loopSize++
        if (value == publicKey) {
            return loopSize
        }
    }
}

fun calculateEncryptionKey(publicKey: Long, loopSize: Int): Long {
    var value = 1L
    repeat(loopSize) {
        value = (value * publicKey) % 20201227
    }
    return value
}