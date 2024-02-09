import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val tlsCount = file.readLines().count { supportsTLS(it) }
    println(tlsCount)
}

fun supportsTLS(ip: String): Boolean {
    val insideBrackets = Regex("\\[[a-z]+\\]").findAll(ip).map { it.value }.toList()

    for (bracketContent in insideBrackets) {
        if (containsABBA(bracketContent)) {
            return false
        }
    }

    val modifiedIp = insideBrackets.fold(ip) { acc, bracketContent -> acc.replace(bracketContent, "-") }
    return containsABBA(modifiedIp)
}

fun containsABBA(s: String): Boolean {
    for (i in 0 until s.length - 3) {
        if (s[i] != s[i + 1] && s[i] == s[i + 3] && s[i + 1] == s[i + 2]) {
            return true
        }
    }
    return false
}