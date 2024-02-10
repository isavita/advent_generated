const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
const steps = parseInt(data);

let currentPos = 0;
let valueAfterZero = 0;

for (let i = 1; i <= 50000000; i++) {
    currentPos = (currentPos + steps) % i;
    if (currentPos === 0) {
        valueAfterZero = i;
    }
    currentPos++;
}

console.log(valueAfterZero);