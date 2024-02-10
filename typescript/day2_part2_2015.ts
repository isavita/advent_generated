
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8');
const lines = input.trim().split('\n');

let totalRibbon = 0;
for (const line of lines) {
    const dimensions = line.split('x');
    if (dimensions.length !== 3) {
        throw new Error('Invalid input format');
    }

    const [l, w, h] = dimensions.map(Number);

    const bow = l * w * h;

    const sides = [l, w, h].sort((a, b) => a - b);
    const wrap = 2 * sides[0] + 2 * sides[1];

    totalRibbon += bow + wrap;
}

console.log(totalRibbon);
