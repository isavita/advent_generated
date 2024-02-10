const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8');
const passports = input.split('\n\n');

let validPassports = 0;
const requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

passports.forEach(passport => {
    if (isValid(passport, requiredFields)) {
        validPassports++;
    }
});

console.log(validPassports);

function isValid(passport, requiredFields) {
    for (let i = 0; i < requiredFields.length; i++) {
        if (!passport.includes(requiredFields[i] + ":")) {
            return false;
        }
    }
    return true;
}