import * as fs from 'fs';

const preambleLength = 25;

function readInput(filePath: string): number[] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.split('\n').map(Number).filter(num => !isNaN(num));
}

function findFirstInvalidNumber(numbers: number[]): number | null {
    for (let i = preambleLength; i < numbers.length; i++) {
        const current = numbers[i];
        const previous = new Set(numbers.slice(i - preambleLength, i));
        let isValid = false;

        for (const num of previous) {
            if (previous.has(current - num) && current - num !== num) {
                isValid = true;
                break;
            }
        }

        if (!isValid) {
            return current;
        }
    }
    return null;
}

const numbers = readInput('input.txt');
const firstInvalidNumber = findFirstInvalidNumber(numbers);

if (firstInvalidNumber !== null) {
    console.log(firstInvalidNumber);
} else {
    console.log('All numbers are valid.');
}