import * as fs from 'fs';

const requiredFields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'];

function isValidPassport(passport: string): boolean {
    const fields = passport.split(/\s+/).map(field => field.split(':')[0]);
    const uniqueFields = new Set(fields);
    return requiredFields.every(field => uniqueFields.has(field));
}

function countValidPassports(input: string): number {
    const passports = input.trim().split(/\n\n/);
    return passports.filter(isValidPassport).length;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf8');
    const validPassportCount = countValidPassports(input);
    console.log(validPassportCount);
}

main();