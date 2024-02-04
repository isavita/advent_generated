const fs = require('fs');

const totalRows = 400000;

function readFirstRow(filename) {
    const data = fs.readFileSync(filename, 'utf8');
    return data.split('\n')[0];
}

function countSafeTiles(firstRow, totalRows) {
    let currentRow = firstRow;
    let safeCount = countChar(currentRow, '.');

    for (let i = 1; i < totalRows; i++) {
        let nextRow = '';
        for (let j = 0; j < currentRow.length; j++) {
            if (isTrap(j - 1, j, j + 1, currentRow)) {
                nextRow += '^';
            } else {
                nextRow += '.';
                safeCount++;
            }
        }
        currentRow = nextRow;
    }
    return safeCount;
}

function isTrap(left, center, right, row) {
    const l = safeIfOutOfBounds(left, row);
    const c = row.charAt(center);
    const r = safeIfOutOfBounds(right, row);

    return (l === '^' && c === '^' && r === '.') ||
        (c === '^' && r === '^' && l === '.') ||
        (l === '^' && c === '.' && r === '.') ||
        (r === '^' && c === '.' && l === '.');
}

function safeIfOutOfBounds(index, row) {
    if (index < 0 || index >= row.length) {
        return '.';
    }
    return row.charAt(index);
}

function countChar(str, char) {
    let count = 0;
    for (let i = 0; i < str.length; i++) {
        if (str.charAt(i) === char) {
            count++;
        }
    }
    return count;
}

const firstRow = readFirstRow("input.txt");
const safeTilesCount = countSafeTiles(firstRow, totalRows);
console.log(safeTilesCount);