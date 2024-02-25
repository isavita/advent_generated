
import strutils

var file = open("input.txt")
var passports: seq[string] = @[]
var passport = ""

while not file.endOfFile:
  let line = file.readLine()
  if line == "":
    passports.add(passport)
    passport = ""
  else:
    passport.add(" " & line)

if passport != "":
  passports.add(passport)

var validPassports = 0
let requiredFields = @["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

proc isValid(passport: string, requiredFields: seq[string]): bool =
  for field in requiredFields:
    if not passport.contains($field & ":"):
      return false
  return true

for p in passports:
  if isValid(p, requiredFields):
    validPassports += 1

echo validPassports
