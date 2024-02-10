const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',');

const lastSpoken = new Map();
let lastNumber, nextNumber;

for (let turn = 1; turn <= 2020; turn++) {
    if (turn - 1 < input.length) {
        lastNumber = parseInt(input[turn - 1]);
        lastSpoken.set(lastNumber, turn);
        continue;
    }
    if (lastSpoken.has(lastNumber) && lastSpoken.get(lastNumber) !== turn - 1) {
        nextNumber = turn - 1 - lastSpoken.get(lastNumber);
    } else {
        nextNumber = 0;
    }
    lastSpoken.set(lastNumber, turn - 1);
    lastNumber = nextNumber;
}

console.log(lastNumber);