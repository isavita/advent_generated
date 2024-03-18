const fs = require('fs');

function validateByr(value) {
  return validateYear(value, 1920, 2002);
}

function validateIyr(value) {
  return validateYear(value, 2010, 2020);
}

function validateEyr(value) {
  return validateYear(value, 2020, 2030);
}

function validateYear(value, min, max) {
  const year = parseInt(value);
  return !isNaN(year) && year >= min && year <= max;
}

function validateHgt(value) {
  if (typeof value === 'string' && value.length > 0) {
    if (value.endsWith('cm')) {
      const hgt = parseInt(value.slice(0, -2));
      return !isNaN(hgt) && hgt >= 150 && hgt <= 193;
    } else if (value.endsWith('in')) {
      const hgt = parseInt(value.slice(0, -2));
      return !isNaN(hgt) && hgt >= 59 && hgt <= 76;
    }
  }
  return false;
}

function validateHcl(value) {
  return /^#[0-9a-f]{6}$/.test(value);
}

function validateEcl(value) {
  const validEcl = ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'];
  return validEcl.includes(value);
}

function validatePid(value) {
  return /^[0-9]{9}$/.test(value);
}

function isValidPassport(passport) {
  const fields = passport.split(/\s+/);
  const fieldMap = new Map();
  for (const field of fields) {
    const [key, value] = field.split(':');
    fieldMap.set(key, value);
  }

  return validateByr(fieldMap.get('byr')) &&
    validateIyr(fieldMap.get('iyr')) &&
    validateEyr(fieldMap.get('eyr')) &&
    validateHgt(fieldMap.get('hgt')) &&
    validateHcl(fieldMap.get('hcl')) &&
    validateEcl(fieldMap.get('ecl')) &&
    validatePid(fieldMap.get('pid'));
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error('Error opening file:', err);
    return;
  }

  const passports = data.trim().split('\n\n');
  let validPassports = 0;

  for (const passport of passports) {
    if (isValidPassport(passport)) {
      validPassports++;
    }
  }

  console.log(validPassports);
});