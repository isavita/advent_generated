import * as fs from 'fs';

interface Passport {
    byr?: string;
    iyr?: string;
    eyr?: string;
    hgt?: string;
    hcl?: string;
    ecl?: string;
    pid?: string;
}

const requiredFields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'] as const;

const isValidYear = (year: string, min: number, max: number): boolean => {
    const num = parseInt(year);
    return year.length === 4 && num >= min && num <= max;
};

const isValidHeight = (height: string): boolean => {
    const match = height.match(/^(\d+)(cm|in)$/);
    if (!match) return false;
    const value = parseInt(match[1]);
    return (match[2] === 'cm' && value >= 150 && value <= 193) ||
           (match[2] === 'in' && value >= 59 && value <= 76);
};

const isValidHairColor = (color: string): boolean => /^#[0-9a-f]{6}$/.test(color);
const isValidEyeColor = (color: string): boolean => ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'].includes(color);
const isValidPassportID = (id: string): boolean => /^\d{9}$/.test(id);

const isValidPassport = (passport: Passport): boolean => {
    return requiredFields.every(field => {
        switch (field) {
            case 'byr': return passport.byr ? isValidYear(passport.byr, 1920, 2002) : false;
            case 'iyr': return passport.iyr ? isValidYear(passport.iyr, 2010, 2020) : false;
            case 'eyr': return passport.eyr ? isValidYear(passport.eyr, 2020, 2030) : false;
            case 'hgt': return passport.hgt ? isValidHeight(passport.hgt) : false;
            case 'hcl': return passport.hcl ? isValidHairColor(passport.hcl) : false;
            case 'ecl': return passport.ecl ? isValidEyeColor(passport.ecl) : false;
            case 'pid': return passport.pid ? isValidPassportID(passport.pid) : false;
            default: return true;
        }
    });
};

const parsePassports = (data: string): Passport[] => {
    return data.split('\n\n').map(block => {
        const passport: Passport = {};
        block.split(/\s+/).forEach(field => {
            const [key, value] = field.split(':');
            passport[key as keyof Passport] = value; // Type assertion here
        });
        return passport;
    });
};

const countValidPassports = (passports: Passport[]): number => {
    return passports.filter(isValidPassport).length;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const passports = parsePassports(data);
    const validCount = countValidPassports(passports);
    console.log(validCount);
};

main();