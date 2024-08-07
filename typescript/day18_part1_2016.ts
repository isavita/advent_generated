import * as fs from 'fs';

// Read input from file
const input = fs.readFileSync('input.txt', 'utf8').trim();

// Function to determine the type of the next tile based on the previous row
function getNextTile(left: string, center: string, right: string): string {
    if ((left === '^' && center === '^' && right === '.') ||
        (center === '^' && right === '^' && left === '.') ||
        (left === '^' && center === '.' && right === '.') ||
        (left === '.' && center === '.' && right === '^')) {
        return '^';
    }
    return '.';
}

// Function to generate the next row based on the previous row
function generateNextRow(row: string): string {
    let nextRow = '';
    for (let i = 0; i < row.length; i++) {
        const left = i > 0 ? row[i - 1] : '.';
        const center = row[i];
        const right = i < row.length - 1 ? row[i + 1] : '.';
        nextRow += getNextTile(left, center, right);
    }
    return nextRow;
}

// Function to count the number of safe tiles in a row
function countSafeTiles(row: string): number {
    return row.split('.').length - 1;
}

// Initialize the first row and the total number of safe tiles
let currentRow = input;
let totalSafeTiles = countSafeTiles(currentRow);

// Generate the next 39 rows and count the safe tiles
for (let i = 1; i < 40; i++) {
    currentRow = generateNextRow(currentRow);
    totalSafeTiles += countSafeTiles(currentRow);
}

// Print the total number of safe tiles
console.log(totalSafeTiles);