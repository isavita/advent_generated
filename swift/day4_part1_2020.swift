
import Foundation

let input = try String(contentsOfFile: "input.txt")
let passports = input.components(separatedBy: "\n\n")

let requiredFields = Set(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])
let optionalField = "cid"

let validPassports = passports.filter { passport in
    let fields = passport.components(separatedBy: CharacterSet(charactersIn: " \n"))
        .map { $0.components(separatedBy: ":")[0] }
    let fieldSet = Set(fields)
    return requiredFields.isSubset(of: fieldSet) || (requiredFields.subtracting([optionalField])).isSubset(of: fieldSet)
}

print(validPassports.count)
