import java.io.File
import java.security.MessageDigest

fun main(args: Array<String>) {
    val salt = File("input.txt").readText().trim()
    var keys = 0
    var index = 0
    while (keys < 64) {
        val hash = getMD5Hash(salt + index.toString())
        val triplet = findTriplet(hash)
        if (triplet != "") {
            for (i in 1..1000) {
                val nextHash = getMD5Hash(salt + (index + i).toString())
                if (nextHash.contains(triplet.repeat(5))) {
                    keys++
                    break
                }
            }
        }
        index++
    }
    println(index - 1)
}

fun getMD5Hash(input: String): String {
    val md5Digest = MessageDigest.getInstance("MD5")
    val byteArray = md5Digest.digest(input.toByteArray())
    return byteArray.joinToString("") { "%02x".format(it) }
}

fun findTriplet(hash: String): String {
    for (i in 0 until hash.length - 2) {
        if (hash[i] == hash[i + 1] && hash[i] == hash[i + 2]) {
            return hash[i].toString()
        }
    }
    return ""
}