const fs = require('fs');

const preambleLength = 25;

const data = fs.readFileSync('input.txt', 'utf8');
const numbers = data.split('\n').map(Number);

for (let i = preambleLength; i < numbers.length; i++) {
    if (!isValid(numbers[i], numbers.slice(i - preambleLength, i))) {
        console.log(numbers[i]);
        break;
    }
}

function isValid(number, previousNumbers) {
    const seen = new Set();
    for (const n of previousNumbers) {
        if (seen.has(number - n)) {
            return true;
        }
        seen.add(n);
    }
    return false;
}