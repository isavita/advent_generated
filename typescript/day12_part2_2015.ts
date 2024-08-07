import * as fs from 'fs';

// Function to sum all numbers in the JSON document
function sumNumbers(data: any, ignoreRed: boolean = false): number {
    let sum = 0;

    function traverse(obj: any): void {
        if (typeof obj === 'number') {
            sum += obj;
        } else if (typeof obj === 'object' && obj !== null) {
            if (Array.isArray(obj)) {
                obj.forEach(traverse);
            } else {
                if (ignoreRed && Object.values(obj).includes('red')) {
                    return;
                }
                Object.values(obj).forEach(traverse);
            }
        }
    }

    traverse(data);
    return sum;
}

// Read the input file
const input = fs.readFileSync('input.txt', 'utf-8');
const data = JSON.parse(input);

// Part One: Sum all numbers in the document
const sumPartOne = sumNumbers(data);
console.log('Part One:', sumPartOne);

// Part Two: Sum all numbers in the document, ignoring objects with the value "red"
const sumPartTwo = sumNumbers(data, true);
console.log('Part Two:', sumPartTwo);