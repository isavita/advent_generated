
import java.io.File
import java.security.MessageDigest

fun main() {
    val salt = File("input.txt").readText().trim()
    var keys = 0
    var index = 0
    val hashCache = mutableMapOf<String, String>()

    while (keys < 64) {
        val hash = getStretchedMD5Hash(salt + index, hashCache)
        val triplet = findTriplet(hash)
        if (triplet.isNotEmpty()) {
            for (i in 1..1000) {
                val nextHash = getStretchedMD5Hash(salt + (index + i), hashCache)
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

fun getStretchedMD5Hash(input: String, cache: MutableMap<String, String>): String {
    return cache.getOrPut(input) {
        var hash = getMD5Hash(input)
        repeat(2016) {
            hash = getMD5Hash(hash)
        }
        hash
    }
}

fun getMD5Hash(input: String): String {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(input.toByteArray())
    return digest.joinToString("") { "%02x".format(it) }
}

fun findTriplet(hash: String): String {
    for (i in 0 until hash.length - 2) {
        if (hash[i] == hash[i + 1] && hash[i] == hash[i + 2]) {
            return hash[i].toString()
        }
    }
    return ""
}
