const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const genAStart = parseInt(input[0]);
const genBStart = parseInt(input[1]);

const genAFactor = 16807;
const genBFactor = 48271;
const modulus = 2147483647;

let genA = genAStart;
let genB = genBStart;
let matches = 0;

for (let i = 0; i < 5000000; i++) {
    // Generate next value for A that is a multiple of 4
    do {
        genA = (genA * genAFactor) % modulus;
    } while (genA % 4 !== 0);

    // Generate next value for B that is a multiple of 8
    do {
        genB = (genB * genBFactor) % modulus;
    } while (genB % 8 !== 0);

    if ((genA & 0xFFFF) === (genB & 0xFFFF)) {
        matches++;
    }
}

console.log(matches);