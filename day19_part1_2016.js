const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const totalElves = parseInt(input);

let highestPowerOfTwo = 1;
while (highestPowerOfTwo * 2 <= totalElves) {
    highestPowerOfTwo *= 2;
}

const winner = (totalElves - highestPowerOfTwo) * 2 + 1;
console.log(winner);