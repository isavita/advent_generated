import * as fs from 'fs';
import * as path from 'path';

// Function to check if a string is nice according to the first set of rules
function isNicePartOne(str: string): boolean {
    const vowels = 'aeiou';
    let vowelCount = 0;
    let hasDoubleLetter = false;
    const forbiddenSubstrings = ['ab', 'cd', 'pq', 'xy'];

    for (let i = 0; i < str.length; i++) {
        if (vowels.includes(str[i])) {
            vowelCount++;
        }
        if (i > 0 && str[i] === str[i - 1]) {
            hasDoubleLetter = true;
        }
    }

    for (const substring of forbiddenSubstrings) {
        if (str.includes(substring)) {
            return false;
        }
    }

    return vowelCount >= 3 && hasDoubleLetter;
}

// Function to check if a string is nice according to the second set of rules
function isNicePartTwo(str: string): boolean {
    let hasRepeatingPair = false;
    let hasRepeatingWithOneBetween = false;

    for (let i = 0; i < str.length - 1; i++) {
        const pair = str.substring(i, i + 2);
        if (str.indexOf(pair, i + 2) !== -1) {
            hasRepeatingPair = true;
        }
    }

    for (let i = 0; i < str.length - 2; i++) {
        if (str[i] === str[i + 2]) {
            hasRepeatingWithOneBetween = true;
        }
    }

    return hasRepeatingPair && hasRepeatingWithOneBetween;
}

// Main function to read the input file and process the strings
function main() {
    const filePath = path.resolve(__dirname, 'input.txt');
    const fileContent = fs.readFileSync(filePath, 'utf-8');
    const strings = fileContent.split('\n').filter(line => line.trim() !== '');

    let niceCountPartOne = 0;
    let niceCountPartTwo = 0;

    for (const str of strings) {
        if (isNicePartOne(str)) {
            niceCountPartOne++;
        }
        if (isNicePartTwo(str)) {
            niceCountPartTwo++;
        }
    }

    console.log(`Number of nice strings (Part One): ${niceCountPartOne}`);
    console.log(`Number of nice strings (Part Two): ${niceCountPartTwo}`);
}

main();