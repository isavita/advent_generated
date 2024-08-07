import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const rows: number = 400000; // Change to 40 for part one
const initialRow: string = input;

const nextRow = (prevRow: string): string => {
    let newRow = '';
    const length = prevRow.length;

    for (let i = 0; i < length; i++) {
        const left = i > 0 ? prevRow[i - 1] : '.';
        const center = prevRow[i];
        const right = i < length - 1 ? prevRow[i + 1] : '.';

        if ((left === '^' && center === '^' && right === '.') ||
            (center === '^' && right === '^' && left === '.') ||
            (left === '^' && center === '.' && right === '.') ||
            (left === '.' && center === '.' && right === '^')) {
            newRow += '^';
        } else {
            newRow += '.';
        }
    }
    return newRow;
};

const countSafeTiles = (initial: string, totalRows: number): number => {
    let currentRow = initial;
    let safeCount = 0;

    for (let i = 0; i < totalRows; i++) {
        safeCount += currentRow.split('').filter(tile => tile === '.').length;
        currentRow = nextRow(currentRow);
    }
    return safeCount;
};

const partOneResult = countSafeTiles(initialRow, 40);
console.log(`Safe tiles in 40 rows: ${partOneResult}`);

const partTwoResult = countSafeTiles(initialRow, rows);
console.log(`Safe tiles in 400000 rows: ${partTwoResult}`);