fs = require 'fs'

isValid = (passport, requiredFields) ->
  for field in requiredFields
    return false unless passport.includes field + ":"
  true

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error opening file:", err
    return

  passports = []
  passport = ""
  lines = data.split '\n'

  for line in lines
    if line.trim() == ""
      passports.push passport.trim()
      passport = ""
    else
      passport += " " + line

  passports.push passport.trim() if passport != ""

  requiredFields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']
  validPassports = 0

  for p in passports
    validPassports++ if isValid p, requiredFields

  console.log validPassports