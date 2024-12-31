
import strutils, sequtils, re, tables

proc validateYear(value: string, min, max: int): bool =
  try:
    let year = parseInt(value)
    return year >= min and year <= max
  except ValueError:
    return false

proc validateHgt(value: string): bool =
  if value.endsWith("cm"):
    try:
      let hgt = parseInt(value.replace("cm", ""))
      return hgt >= 150 and hgt <= 193
    except ValueError:
      return false
  elif value.endsWith("in"):
    try:
      let hgt = parseInt(value.replace("in", ""))
      return hgt >= 59 and hgt <= 76
    except ValueError:
      return false
  else:
    return false

proc validateHcl(value: string): bool =
  value.match(re"^#[0-9a-f]{6}$")

proc validateEcl(value: string): bool =
  let validEcl = {"amb": true, "blu": true, "brn": true, "gry": true, "grn": true, "hzl": true, "oth": true}.toTable
  return validEcl.contains(value)

proc validatePid(value: string): bool =
  value.match(re"^[0-9]{9}$")

proc isValidPassport(passport: string): bool =
  let fields = passport.split(" ")
  var fieldMap: Table[string, string]
  for field in fields:
    let parts = field.split(":")
    if parts.len == 2:
      fieldMap[parts[0]] = parts[1]

  return fieldMap.contains("byr") and validateYear(fieldMap["byr"], 1920, 2002) and
         fieldMap.contains("iyr") and validateYear(fieldMap["iyr"], 2010, 2020) and
         fieldMap.contains("eyr") and validateYear(fieldMap["eyr"], 2020, 2030) and
         fieldMap.contains("hgt") and validateHgt(fieldMap["hgt"]) and
         fieldMap.contains("hcl") and validateHcl(fieldMap["hcl"]) and
         fieldMap.contains("ecl") and validateEcl(fieldMap["ecl"]) and
         fieldMap.contains("pid") and validatePid(fieldMap["pid"])

var passports: seq[string] = @[]
var passport = ""
for line in lines("input.txt"):
  if line.len == 0:
    passports.add(passport.strip())
    passport = ""
  else:
    passport = passport & " " & line
if passport.len > 0:
  passports.add(passport.strip())

var validPassports = 0
for p in passports:
  if isValidPassport(p):
    inc validPassports

echo validPassports
