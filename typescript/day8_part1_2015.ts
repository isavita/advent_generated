import * as fs from 'fs';
import * as path from 'path';

// Function to calculate the number of characters in memory for a string literal
function countInMemoryCharacters(str: string): number {
    let count = 0;
    let i = 0;
    while (i < str.length) {
        if (str[i] === '\\') {
            if (str[i + 1] === '\\' || str[i + 1] === '"') {
                i += 2;
            } else if (str[i + 1] === 'x') {
                i += 4;
            }
            count++;
        } else {
            i++;
            count++;
        }
    }
    // Subtract 2 for the surrounding quotes
    return count - 2;
}

// Function to calculate the number of characters of code for a string literal
function countCodeCharacters(str: string): number {
    return str.length;
}

// Main function to read input, process it, and print the result
function main(): void {
    const filePath = path.join(__dirname, 'input.txt');
    const input = fs.readFileSync(filePath, 'utf-8').trim().split('\n');

    let totalCodeCharacters = 0;
    let totalInMemoryCharacters = 0;

    for (const line of input) {
        totalCodeCharacters += countCodeCharacters(line);
        totalInMemoryCharacters += countInMemoryCharacters(line);
    }

    const result = totalCodeCharacters - totalInMemoryCharacters;
    console.log(result);
}

main();