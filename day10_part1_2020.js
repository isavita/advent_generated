const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);

const adapters = data.sort((a, b) => a - b);
const joltDifferences = { 3: 1 };
let previousJoltage = 0;

for (const adapter of adapters) {
    const diff = adapter - previousJoltage;
    joltDifferences[diff] = (joltDifferences[diff] || 0) + 1;
    previousJoltage = adapter;
}

const product = joltDifferences[1] * joltDifferences[3];
console.log(product);