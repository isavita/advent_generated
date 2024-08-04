import java.io.File
import java.math.BigInteger
import java.security.MessageDigest

fun main() {
    val doorID = File("input.txt").readText().trim()
    val password = findPassword(doorID)
    println(password)
}

fun findPassword(doorID: String): String {
    var password = ""
    var i = 0
    while (password.length < 8) {
        val hash = md5Hash(doorID + i)
        if (hash.startsWith("00000")) {
            password += hash[5]
        }
        i++
    }
    return password
}

fun md5Hash(input: String): String {
    val md = MessageDigest.getInstance("MD5")
    val messageDigest = md.digest(input.toByteArray())
    val bigInt = BigInteger(1, messageDigest)
    return bigInt.toString(16).padStart(32, '0')
}