const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let totalRibbon = 0;

input.forEach(line => {
    const dimensions = line.split('x');
    if (dimensions.length !== 3) {
        console.error('Invalid input format');
        process.exit(1);
    }

    const l = parseInt(dimensions[0]);
    const w = parseInt(dimensions[1]);
    const h = parseInt(dimensions[2]);

    const bow = l * w * h;

    const sides = [l, w, h].sort((a, b) => a - b);
    const wrap = 2 * sides[0] + 2 * sides[1];

    totalRibbon += bow + wrap;
});

console.log(totalRibbon);