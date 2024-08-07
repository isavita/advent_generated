import * as fs from 'fs';

const inputFilePath = 'input.txt';

const getCodeAtPosition = (row: number, col: number): number => {
    const initialCode = 20151125;
    const multiplier = 252533;
    const modulus = 33554393;

    let code = initialCode;
    let currentRow = 1;
    let currentCol = 1;

    while (currentRow !== row || currentCol !== col) {
        code = (code * multiplier) % modulus;
        if (currentRow === 1) {
            currentRow = currentCol + 1;
            currentCol = 1;
        } else {
            currentRow--;
            currentCol++;
        }
    }

    return code;
};

const main = () => {
    const input = fs.readFileSync(inputFilePath, 'utf-8').trim();
    const [row, col] = input.match(/\d+/g)?.map(Number) || [1, 1];

    const code = getCodeAtPosition(row, col);
    console.log(code);
};

main();