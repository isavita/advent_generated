def input = new File("input.txt").text

def passports = input.split("\n\n")

def numValid = 0

passports.each { passport ->
    def fields = passport.split("\\s+")

    def passportMap = [:]

    fields.each { field ->
        def keyValue = field.split(":")
        passportMap[keyValue[0]] = keyValue[1]
    }

    if (passportMap.size() == 8 || (passportMap.size() == 7 && !passportMap.containsKey("cid"))) {
        def valid = true

        if (passportMap.byr.toInteger() < 1920 || passportMap.byr.toInteger() > 2002) {
            valid = false
        }

        if (passportMap.iyr.toInteger() < 2010 || passportMap.iyr.toInteger() > 2020) {
            valid = false
        }

        if (passportMap.eyr.toInteger() < 2020 || passportMap.eyr.toInteger() > 2030) {
            valid = false
        }

        if (!passportMap.hgt.matches("\\d+(cm|in)")) {
            valid = false
        } else {
            def height = passportMap.hgt.take(passportMap.hgt.size() - 2).toInteger()
            if (passportMap.hgt.endsWith("cm") && (height < 150 || height > 193)) {
                valid = false
            } else if (passportMap.hgt.endsWith("in") && (height < 59 || height > 76)) {
                valid = false
            }
        }

        if (!passportMap.hcl.matches("#[0-9a-f]{6}")) {
            valid = false
        }

        if (!["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(passportMap.ecl)) {
            valid = false
        }

        if (!passportMap.pid.matches("\\d{9}")) {
            valid = false
        }

        if (valid) {
            numValid++
        }
    }
}

println numValid