const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const genAStart = parseInt(input[0]);
const genBStart = parseInt(input[1]);

const genAFactor = 16807;
const genBFactor = 48271;
const modulus = 2147483647;

let genA = genAStart;
let genB = genBStart;
let matches = 0;

for (let i = 0; i < 40000000; i++) {
    genA = (genA * genAFactor) % modulus;
    genB = (genB * genBFactor) % modulus;

    if ((genA & 0xFFFF) === (genB & 0xFFFF)) {
        matches++;
    }
}

console.log(matches);