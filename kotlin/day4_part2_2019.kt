import java.io.File

fun main() {
    val input = File("input.txt").readLines()[0].split("-").map { it.toInt() }
    val start = input[0]
    val end = input[1]

    var count1 = 0
    var count2 = 0

    for (i in start..end) {
        val password = i.toString()
        if (isValidPasswordPart1(password)) {
            count1++
        }
        if (isValidPasswordPart2(password)) {
            count2++
        }
    }

    println(count1)
    println(count2)
}

fun isValidPasswordPart1(password: String): Boolean {
    var hasAdjacent = false
    var neverDecrease = true

    for (i in 0 until password.length - 1) {
        if (password[i] == password[i + 1]) {
            hasAdjacent = true
        }
        if (password[i] > password[i + 1]) {
            neverDecrease = false
            break
        }
    }

    return hasAdjacent && neverDecrease
}

fun isValidPasswordPart2(password: String): Boolean {
    var hasAdjacent = false
    var neverDecrease = true
    var adjacentCount = 1

    for (i in 0 until password.length - 1) {
        if (password[i] == password[i + 1]) {
            adjacentCount++
        } else {
            if (adjacentCount == 2) {
                hasAdjacent = true
            }
            adjacentCount = 1
        }
        if (password[i] > password[i + 1]) {
            neverDecrease = false
            break
        }
    }

    if (adjacentCount == 2) {
        hasAdjacent = true
    }

    return hasAdjacent && neverDecrease
}