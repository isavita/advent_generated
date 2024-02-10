const fs = require('fs');

function readFileToMatrix(filePath) {
    const data = fs.readFileSync(filePath, 'utf8').split('\n');
    const matrix = data.map(row => row.split(''));
    return matrix;
}

function sumOfPartNumbers(matrix) {
    let sum = 0;
    const visited = new Array(matrix.length).fill().map(() => new Array(matrix[0].length).fill(false));

    for (let y = 0; y < matrix.length; y++) {
        for (let x = 0; x < matrix[y].length; x++) {
            if (!visited[y][x] && !isNaN(parseInt(matrix[y][x]))) {
                const { number, length } = extractNumber(matrix, x, y);
                if (isAdjacentToSymbol(matrix, x, y, length)) {
                    sum += number;
                }
                for (let i = 0; i < length; i++) {
                    visited[y][x + i] = true;
                }
            }
        }
    }
    return sum;
}

function extractNumber(matrix, x, y) {
    let numberStr = '';
    while (x < matrix[y].length && !isNaN(parseInt(matrix[y][x]))) {
        numberStr += matrix[y][x];
        x++;
    }
    const number = parseInt(numberStr);
    return { number, length: numberStr.length };
}

function isAdjacentToSymbol(matrix, x, y, length) {
    for (let i = 0; i < length; i++) {
        if (checkAdjacent(matrix, x + i, y)) {
            return true;
        }
    }
    return false;
}

function checkAdjacent(matrix, x, y) {
    for (let dy = -1; dy <= 1; dy++) {
        for (let dx = -1; dx <= 1; dx++) {
            const adjX = x + dx;
            const adjY = y + dy;
            if (adjY >= 0 && adjY < matrix.length && adjX >= 0 && adjX < matrix[adjY].length) {
                if (isNaN(parseInt(matrix[adjY][adjX])) && matrix[adjY][adjX] !== '.') {
                    return true;
                }
            }
        }
    }
    return false;
}

const matrix = readFileToMatrix('input.txt');
const sum = sumOfPartNumbers(matrix);
console.log(sum);