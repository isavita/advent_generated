const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number);

const spoken = new Map();

let lastSpoken = 0;

for (let i = 0; i < 30000000; i++) {
    if (i < input.length) {
        spoken.set(input[i], i);
        lastSpoken = input[i];
    } else {
        const lastSpokenIndex = spoken.get(lastSpoken);
        spoken.set(lastSpoken, i - 1);
        lastSpoken = lastSpokenIndex === undefined ? 0 : i - 1 - lastSpokenIndex;
    }
}

console.log(lastSpoken);