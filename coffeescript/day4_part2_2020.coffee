fs = require 'fs'

isValidPassport = (passport) ->
  fields = passport.split(/\s+/)
  fieldMap = {}
  for field in fields
    [key, value] = field.split(':')
    fieldMap[key] = value

  validateByr(fieldMap['byr']) and
  validateIyr(fieldMap['iyr']) and
  validateEyr(fieldMap['eyr']) and
  validateHgt(fieldMap['hgt']) and
  validateHcl(fieldMap['hcl']) and
  validateEcl(fieldMap['ecl']) and
  validatePid(fieldMap['pid'])

validateYear = (value, min, max) ->
  year = parseInt(value, 10)
  year >= min and year <= max

validateByr = (value) -> validateYear(value, 1920, 2002)
validateIyr = (value) -> validateYear(value, 2010, 2020)
validateEyr = (value) -> validateYear(value, 2020, 2030)

validateHgt = (value) ->
  return false unless value?
  if value.endsWith('cm')
    hgt = parseInt(value.slice(0, -2), 10)
    hgt >= 150 and hgt <= 193
  else if value.endsWith('in')
    hgt = parseInt(value.slice(0, -2), 10)
    hgt >= 59 and hgt <= 76
  else
    false

validateHcl = (value) -> /^#[0-9a-f]{6}$/.test(value)

validateEcl = (value) -> value in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']

validatePid = (value) -> /^[0-9]{9}$/.test(value)

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log 'Error opening file:', err
    return

  passports = data.split(/\n\s*\n/).map (p) -> p.trim()
  validPassports = 0

  for passport in passports
    if isValidPassport(passport)
      validPassports++

  console.log validPassports