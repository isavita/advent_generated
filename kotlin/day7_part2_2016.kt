import java.io.File

fun main() {
    val input = File("input.txt").readLines()

    var tlsCount = 0
    var sslCount = 0

    input.forEach { ip ->
        if (supportsTLS(ip)) {
            tlsCount++
        }
        if (supportsSSL(ip)) {
            sslCount++
        }
    }

    println("TLS count: $tlsCount")
    println("SSL count: $sslCount")
}

fun supportsTLS(ip: String): Boolean {
    val parts = ip.split("[", "]")
    var abbaOutside = false
    var abbaInside = false

    for (i in parts.indices) {
        if (i % 2 == 0) {
            if (hasABBA(parts[i])) {
                abbaOutside = true
            }
        } else {
            if (hasABBA(parts[i])) {
                abbaInside = true
            }
        }
    }

    return abbaOutside && !abbaInside
}

fun hasABBA(s: String): Boolean {
    for (i in 0 until s.length - 3) {
        if (s[i] == s[i + 3] && s[i + 1] == s[i + 2] && s[i] != s[i + 1]) {
            return true
        }
    }
    return false
}

fun supportsSSL(ip: String): Boolean {
    val parts = ip.split("[", "]")
    val abas = mutableListOf<String>()
    val babs = mutableListOf<String>()

    for (i in parts.indices) {
        if (i % 2 == 0) {
            abas.addAll(getABAs(parts[i]))
        } else {
            babs.addAll(getABAs(parts[i]))
        }
    }

    for (aba in abas) {
        val bab = aba[1].toString() + aba[0] + aba[1]
        if (babs.contains(bab)) {
            return true
        }
    }

    return false
}

fun getABAs(s: String): List<String> {
    val abas = mutableListOf<String>()

    for (i in 0 until s.length - 2) {
        if (s[i] == s[i + 2] && s[i] != s[i + 1]) {
            abas.add(s.substring(i, i + 3))
        }
    }

    return abas
}