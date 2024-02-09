import java.io.File

fun main(args: Array<String>) {
    val currentPassword = readInput("input.txt")
    val newPassword = findNextPassword(currentPassword)
    println(newPassword)
}

fun readInput(filename: String): String {
    return File(filename).readLines()[0]
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
    var newPassword = password.toCharArray()
    for (i in newPassword.size - 1 downTo 0) {
        newPassword[i]++
        if (newPassword[i] > 'z') {
            newPassword[i] = 'a'
        } else {
            break
        }
    }
    return String(newPassword)
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
            i += 2
        } else {
            i++
        }
    }
    return count >= 2
}