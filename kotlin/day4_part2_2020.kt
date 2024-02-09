import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val passports = file.readLines().joinToString(" ").split("  ")

    var validPassports = 0

    for (passport in passports) {
        if (isValidPassport(passport)) {
            validPassports++
        }
    }

    println(validPassports)
}

fun isValidPassport(passport: String): Boolean {
    val fields = passport.split(" ")
    val fieldMap = fields.associate {
        val parts = it.split(":")
        parts[0] to parts[1]
    }

    return validateByr(fieldMap["byr"]) &&
            validateIyr(fieldMap["iyr"]) &&
            validateEyr(fieldMap["eyr"]) &&
            validateHgt(fieldMap["hgt"]) &&
            validateHcl(fieldMap["hcl"]) &&
            validateEcl(fieldMap["ecl"]) &&
            validatePid(fieldMap["pid"])
}

fun validateByr(value: String?): Boolean {
    return validateYear(value, 1920, 2002)
}

fun validateIyr(value: String?): Boolean {
    return validateYear(value, 2010, 2020)
}

fun validateEyr(value: String?): Boolean {
    return validateYear(value, 2020, 2030)
}

fun validateYear(value: String?, min: Int, max: Int): Boolean {
    val year = value?.toIntOrNull() ?: return false
    return year in min..max
}

fun validateHgt(value: String?): Boolean {
    return when {
        value == null -> false
        value.endsWith("cm") -> {
            val hgt = value.dropLast(2).toIntOrNull() ?: return false
            hgt in 150..193
        }
        value.endsWith("in") -> {
            val hgt = value.dropLast(2).toIntOrNull() ?: return false
            hgt in 59..76
        }
        else -> false
    }
}

fun validateHcl(value: String?): Boolean {
    return value?.matches(Regex("#[0-9a-f]{6}")) ?: false
}

fun validateEcl(value: String?): Boolean {
    val validEcl = setOf("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    return value in validEcl
}

fun validatePid(value: String?): Boolean {
    return value?.matches(Regex("[0-9]{9}")) ?: false
}