import java.io.File

fun main(args: Array<String>) {
    val currentPassword = readInput("input.txt")
    if (currentPassword.isEmpty()) {
        println("Error reading input")
        return
    }

    val firstNewPassword = findNextPassword(currentPassword)
    val secondNewPassword = findNextPassword(firstNewPassword)
    println(secondNewPassword)
}

fun readInput(filename: String): String {
    return File(filename).readLines().firstOrNull() ?: ""
}

fun findNextPassword(password: String): String {
    var newPassword = password
    while (true) {
        newPassword = incrementPassword(newPassword)
        if (isValidPassword(newPassword)) {
            break
        }
    }
    return newPassword
}

fun incrementPassword(password: String): String {
    val chars = password.toCharArray()
    for (i in chars.indices.reversed()) {
        chars[i]++
        if (chars[i] > 'z') {
            chars[i] = 'a'
        } else {
            break
        }
    }
    return String(chars)
}

fun isValidPassword(password: String): Boolean {
    return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password)
}

fun hasStraight(password: String): Boolean {
    for (i in 0 until password.length - 2) {
        if (password[i] + 1 == password[i + 1] && password[i] + 2 == password[i + 2]) {
            return true
        }
    }
    return false
}

fun containsInvalidLetters(password: String): Boolean {
    return password.contains('i') || password.contains('o') || password.contains('l')
}

fun hasTwoPairs(password: String): Boolean {
    var count = 0
    var i = 0
    while (i < password.length - 1) {
        if (password[i] == password[i + 1]) {
            count++
            i++ // Skip the next character
        }
        i++
    }
    return count >= 2
}