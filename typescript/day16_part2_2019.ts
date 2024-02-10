
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const repeatedInput = repeatInput(input, 10000);

const offset = +input.slice(0, 7);

for (let phase = 0; phase < 100; phase++) {
    let sum = 0;
    for (let i = repeatedInput.length - 1; i >= offset; i--) {
        sum += repeatedInput[i];
        repeatedInput[i] = sum % 10;
    }
}

let message = '';
for (let i = offset; i < offset + 8; i++) {
    message += repeatedInput[i];
}

console.log(message);

function repeatInput(input, times) {
    const digits = [];
    for (let t = 0; t < times; t++) {
        for (let i = 0; i < input.length; i++) {
            const digit = parseInt(input[i]);
            digits.push(digit);
        }
    }
    return digits;
}
