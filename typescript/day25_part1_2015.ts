const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').toString();

const re = /row (\d+), column (\d+)/;
const matches = data.match(re);

if (matches && matches.length === 3) {
    const row = parseInt(matches[1]);
    const column = parseInt(matches[2]);

    const pos = getPosition(row, column);
    const code = getCode(pos);

    console.log(code);
} else {
    console.log("Invalid input format.");
}

function getPosition(row, column) {
    return (row + column - 2) * (row + column - 1) / 2 + column;
}

function getCode(position) {
    const startCode = 20151125;
    const multiplier = 252533;
    const modulus = 33554393;

    let code = startCode;
    for (let i = 1; i < position; i++) {
        code = (code * multiplier) % modulus;
    }
    return code;
}