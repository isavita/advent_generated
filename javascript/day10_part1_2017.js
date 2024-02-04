const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const lengths = input.split(',').map(Number);

const list = Array.from({ length: 256 }, (_, i) => i);
let currentPosition = 0;
let skipSize = 0;

for (const length of lengths) {
    for (let i = 0; i < length / 2; i++) {
        const start = (currentPosition + i) % 256;
        const end = (currentPosition + length - 1 - i) % 256;
        [list[start], list[end]] = [list[end], list[start]];
    }

    currentPosition = (currentPosition + length + skipSize) % 256;
    skipSize++;
}

const result = list[0] * list[1];
console.log(result);