def input = new File("input.txt").text.split("\n\n")

def isValidPassport(passport) {
    def fields = passport.split().collect { it.split(":")[0] }
    return fields.containsAll(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])
}

def validPassports = input.count { isValidPassport(it) }

println validPassports