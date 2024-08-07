import * as fs from 'fs';

// Function to read the content of input.txt
function readInputFile(filePath: string): string {
    return fs.readFileSync(filePath, 'utf-8');
}

// Function to traverse the JSON structure and sum all numbers
function sumNumbers(obj: any): number {
    let sum = 0;

    if (typeof obj === 'number') {
        sum += obj;
    } else if (Array.isArray(obj)) {
        for (const item of obj) {
            sum += sumNumbers(item);
        }
    } else if (typeof obj === 'object' && obj !== null) {
        for (const key in obj) {
            if (obj.hasOwnProperty(key)) {
                sum += sumNumbers(obj[key]);
            }
        }
    }

    return sum;
}

// Main function to execute the program
function main() {
    const filePath = 'input.txt';
    const inputContent = readInputFile(filePath);
    const jsonObj = JSON.parse(inputContent);
    const totalSum = sumNumbers(jsonObj);

    console.log(totalSum);
}

// Run the main function
main();