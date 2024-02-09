import java.io.File
import java.security.MessageDigest

fun main() {
    val secretKey = File("input.txt").readText().trim()

    var number = 1
    while (true) {
        val input = secretKey + number
        val md5 = MessageDigest.getInstance("MD5").digest(input.toByteArray()).joinToString("") { "%02x".format(it) }
        if (md5.startsWith("000000")) {
            println(number)
            break
        }
        number++
    }
}