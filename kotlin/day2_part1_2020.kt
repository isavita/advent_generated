import java.io.File

fun validatePassword(policy: String, password: String): Boolean {
    val (min, max, char) = policy.split("-", " ", ":")
    val count = password.count { it == char.first() }
    return count in min.toInt()..max.toInt()
}

fun main(args: Array<String>) {
    var validCount = 0
    File("input.txt").forEachLine { line ->
        val (policy, password) = line.split(":")
        if (validatePassword(policy, password.trim())) {
            validCount++
        }
    }
    println(validCount)
}