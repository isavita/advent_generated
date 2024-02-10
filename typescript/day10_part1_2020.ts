
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);
const adapters = input.sort((a, b) => a - b);

const joltDifferences = { 1: 0, 2: 0, 3: 1 };
let previousJoltage = 0;

adapters.forEach((adapter) => {
    const diff = adapter - previousJoltage;
    joltDifferences[diff]++;
    previousJoltage = adapter;
});

const product = joltDifferences[1] * joltDifferences[3];
console.log(product);
