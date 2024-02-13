
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let passports = input.components(separatedBy: "\n\n")

var validPassports = 0

for passport in passports {
    if isValidPassport(passport) {
        validPassports += 1
    }
}

print(validPassports)

func isValidPassport(_ passport: String) -> Bool {
    var fieldMap = [String: String]()
    let fields = passport.components(separatedBy: CharacterSet.whitespacesAndNewlines)
    
    for field in fields {
        let parts = field.components(separatedBy: ":")
        fieldMap[parts[0]] = parts[1]
    }
    
    return validateByr(fieldMap["byr"] ?? "") &&
        validateIyr(fieldMap["iyr"] ?? "") &&
        validateEyr(fieldMap["eyr"] ?? "") &&
        validateHgt(fieldMap["hgt"] ?? "") &&
        validateHcl(fieldMap["hcl"] ?? "") &&
        validateEcl(fieldMap["ecl"] ?? "") &&
        validatePid(fieldMap["pid"] ?? "")
}

func validateYear(_ value: String, min: Int, max: Int) -> Bool {
    guard let year = Int(value) else { return false }
    return year >= min && year <= max
}

func validateByr(_ value: String) -> Bool {
    return validateYear(value, min: 1920, max: 2002)
}

func validateIyr(_ value: String) -> Bool {
    return validateYear(value, min: 2010, max: 2020)
}

func validateEyr(_ value: String) -> Bool {
    return validateYear(value, min: 2020, max: 2030)
}

func validateHgt(_ value: String) -> Bool {
    if value.hasSuffix("cm") {
        guard let hgt = Int(value.replacingOccurrences(of: "cm", with: "")) else { return false }
        return hgt >= 150 && hgt <= 193
    } else if value.hasSuffix("in") {
        guard let hgt = Int(value.replacingOccurrences(of: "in", with: "")) else { return false }
        return hgt >= 59 && hgt <= 76
    }
    return false
}

func validateHcl(_ value: String) -> Bool {
    let regex = try! NSRegularExpression(pattern: "^#[0-9a-f]{6}$")
    let range = NSRange(location: 0, length: value.utf16.count)
    return regex.firstMatch(in: value, options: [], range: range) != nil
}

func validateEcl(_ value: String) -> Bool {
    let validEcl = Set(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    return validEcl.contains(value)
}

func validatePid(_ value: String) -> Bool {
    let regex = try! NSRegularExpression(pattern: "^[0-9]{9}$")
    let range = NSRange(location: 0, length: value.utf16.count)
    return regex.firstMatch(in: value, options: [], range: range) != nil
}
