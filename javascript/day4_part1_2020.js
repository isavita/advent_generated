const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let passports = [];
let passport = '';

for (let line of input) {
    if (line === '') {
        passports.push(passport);
        passport = '';
    } else {
        passport += ' ' + line;
    }
}
if (passport !== '') {
    passports.push(passport);
}

let validPassports = 0;
const requiredFields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'];

for (let p of passports) {
    if (isValid(p, requiredFields)) {
        validPassports++;
    }
}

console.log(validPassports);

function isValid(passport, requiredFields) {
    for (let field of requiredFields) {
        if (!passport.includes(field + ':')) {
            return false;
        }
    }
    return true;
}