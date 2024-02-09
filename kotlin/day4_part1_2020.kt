import java.io.File

fun main() {
    val passports = File("input.txt").readText().split("\n\n")
    val requiredFields = setOf("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    val validPassports = passports.count { passport ->
        val fields = passport.split("\\s".toRegex()).map { it.split(":")[0] }.toSet()
        requiredFields.all { it in fields }
    }

    println(validPassports)
}