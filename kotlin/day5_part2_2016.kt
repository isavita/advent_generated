
import java.io.File
import java.security.MessageDigest

fun main() {
    val doorId = File("input.txt").readText().trim()
    val password = findPassword(doorId)
    println(password)
}

fun findPassword(doorId: String): String {
    val password = CharArray(8)
    var filledPositions = 0
    val found = BooleanArray(8)
    var i = 0
    while (filledPositions < 8) {
        val hash = md5Hash(doorId + i)
        if (hash.startsWith("00000")) {
            val pos = hash[5]
            if (pos in '0'..'7') {
                val posIndex = pos - '0'
                if (!found[posIndex]) {
                    found[posIndex] = true
                    password[posIndex] = hash[6]
                    filledPositions++
                }
            }
        }
        i++
    }
    return String(password)
}

fun md5Hash(input: String): String {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(input.toByteArray())
    return digest.joinToString("") { "%02x".format(it) }
}
