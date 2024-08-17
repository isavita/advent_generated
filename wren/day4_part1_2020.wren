import "io" for File

var isValid = Fn.new { |passport, requiredFields|
  for (field in requiredFields) {
    if (!passport.contains(field + ":")) return false
  }
  return true
}

var main = Fn.new {
  var input = File.read("input.txt").trim()
  var passports = input.split("\n\n")

  var validPassports = 0
  var requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

  for (passport in passports) {
    if (isValid.call(passport, requiredFields)) validPassports = validPassports + 1
  }

  System.print(validPassports)
}

main.call()